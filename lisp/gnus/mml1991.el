;;; mml1991.el --- Old PGP message format (RFC 1991) support for MML

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Sascha Lüdecke <sascha@meta-x.de>,
;;	Simon Josefsson <simon@josefsson.org> (Mailcrypt interface, Gnus glue)
;; Keywords: PGP

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

(eval-and-compile
  ;; For Emacs <22.2 and XEmacs.
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r)))

  (if (locate-library "password-cache")
      (require 'password-cache)
    (require 'password)))

(eval-when-compile
  (require 'cl)
  (require 'mm-util))

(require 'mm-encode)
(require 'mml-sec)

(defvar mc-pgp-always-sign)

(autoload 'quoted-printable-decode-region "qp")
(autoload 'quoted-printable-encode-region "qp")

(autoload 'mm-decode-content-transfer-encoding "mm-bodies")
(autoload 'mm-encode-content-transfer-encoding "mm-bodies")
(autoload 'message-options-get "message")
(autoload 'message-options-set "message")

(defvar mml1991-use mml2015-use
  "The package used for PGP.")

(defvar mml1991-function-alist
  '((mailcrypt mml1991-mailcrypt-sign
	       mml1991-mailcrypt-encrypt)
    (pgg mml1991-pgg-sign
	 mml1991-pgg-encrypt)
    (epg mml1991-epg-sign
	 mml1991-epg-encrypt))
  "Alist of PGP functions.")

(defvar mml1991-cache-passphrase mml-secure-cache-passphrase
  "If t, cache passphrase.")

(defvar mml1991-passphrase-cache-expiry mml-secure-passphrase-cache-expiry
  "How many seconds the passphrase is cached.
Whether the passphrase is cached at all is controlled by
`mml1991-cache-passphrase'.")

(defvar mml1991-signers nil
  "A list of your own key ID which will be used to sign a message.")

(defvar mml1991-encrypt-to-self nil
  "If t, add your own key ID to recipient list when encryption.")

;;; mailcrypt wrapper

(autoload 'mc-sign-generic "mc-toplev")

(defvar mml1991-decrypt-function 'mailcrypt-decrypt)
(defvar mml1991-verify-function 'mailcrypt-verify)

(defun mml1991-mailcrypt-sign (cont)
  (let ((text (current-buffer))
	headers signature
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Save MIME Content[^ ]+: headers from signing
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (unless (bobp)
      (setq headers (buffer-string))
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (quoted-printable-decode-region (point-min) (point-max))
    (with-temp-buffer
      (setq signature (current-buffer))
      (insert-buffer-substring text)
      (unless (mc-sign-generic (message-options-get 'message-sender)
			       nil nil nil nil)
	(unless (> (point-max) (point-min))
	  (pop-to-buffer result-buffer)
	  (error "Sign error")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (quoted-printable-encode-region (point-min) (point-max))
      (set-buffer text)
      (delete-region (point-min) (point-max))
      (if headers (insert headers))
      (insert "\n")
      (insert-buffer-substring signature)
      (goto-char (point-max)))))

(declare-function mc-encrypt-generic "ext:mc-toplev"
                  (&optional recipients scheme start end from sign))

(defun mml1991-mailcrypt-encrypt (cont &optional sign)
  (let ((text (current-buffer))
	(mc-pgp-always-sign
	 (or mc-pgp-always-sign
	     sign
	     (eq t (or (message-options-get 'message-sign-encrypt)
		       (message-options-set
			'message-sign-encrypt
			(or (y-or-n-p "Sign the message? ")
			    'not))))
	     'never))
	cipher
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Strip MIME Content[^ ]: headers since it will be ASCII ARMORED
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (unless (bobp)
      (delete-region (point-min) (point)))
    (with-temp-buffer
      (inline (mm-disable-multibyte))
      (setq cipher (current-buffer))
      (insert-buffer-substring text)
      (unless (mc-encrypt-generic
               (or
                (message-options-get 'message-recipients)
                (message-options-set 'message-recipients
                                     (read-string "Recipients: ")))
               nil
               (point-min) (point-max)
               (message-options-get 'message-sender)
               'sign)
        (unless (> (point-max) (point-min))
          (pop-to-buffer result-buffer)
          (error "Encrypt error")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
        (replace-match "" t t))
      (set-buffer text)
      (delete-region (point-min) (point-max))
      ;;(insert "Content-Type: application/pgp-encrypted\n\n")
      ;;(insert "Version: 1\n\n")
      (insert "\n")
      (insert-buffer-substring cipher)
      (goto-char (point-max)))))

;; pgg wrapper

(autoload 'pgg-sign-region "pgg")
(autoload 'pgg-encrypt-region "pgg")

(defvar pgg-default-user-id)
(defvar pgg-errors-buffer)
(defvar pgg-output-buffer)

(defun mml1991-pgg-sign (cont)
  (let ((pgg-text-mode t)
	(pgg-default-user-id (or (message-options-get 'mml-sender)
				 pgg-default-user-id))
	headers cte)
    ;; Don't sign headers.
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (setq headers (buffer-substring (point-min) (point)))
      (save-restriction
	(narrow-to-region (point-min) (point))
	(setq cte (mail-fetch-field "content-transfer-encoding")))
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(setq cte (intern (downcase cte)))
	(mm-decode-content-transfer-encoding cte)))
    (unless (pgg-sign-region (point-min) (point-max) t)
      (pop-to-buffer pgg-errors-buffer)
      (error "Encrypt error"))
    (delete-region (point-min) (point-max))
    (mm-with-unibyte-current-buffer
      (insert-buffer-substring pgg-output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (when cte
	(mm-encode-content-transfer-encoding cte))
      (goto-char (point-min))
      (when headers
	(insert headers))
      (insert "\n"))
    t))

(defun mml1991-pgg-encrypt (cont &optional sign)
  (goto-char (point-min))
  (when (re-search-forward "^$" nil t)
    (let ((cte (save-restriction
		 (narrow-to-region (point-min) (point))
		 (mail-fetch-field "content-transfer-encoding"))))
      ;; Strip MIME headers since it will be ASCII armored.
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(mm-decode-content-transfer-encoding (intern (downcase cte))))))
  (unless (let ((pgg-text-mode t))
	    (pgg-encrypt-region
	     (point-min) (point-max)
	     (split-string
	      (or
	       (message-options-get 'message-recipients)
	       (message-options-set 'message-recipients
				    (read-string "Recipients: ")))
	      "[ \f\t\n\r\v,]+")
	     sign))
    (pop-to-buffer pgg-errors-buffer)
    (error "Encrypt error"))
  (delete-region (point-min) (point-max))
  (insert "\n")
  (insert-buffer-substring pgg-output-buffer)
  t)

;; epg wrapper

(defvar epg-user-id-alist)

(autoload 'epg-make-context "epg")
(autoload 'epg-passphrase-callback-function "epg")
(autoload 'epa-select-keys "epa")
(autoload 'epg-list-keys "epg")
(autoload 'epg-context-set-armor "epg")
(autoload 'epg-context-set-textmode "epg")
(autoload 'epg-context-set-signers "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-key-sub-key-list "epg")
(autoload 'epg-sub-key-capability "epg")
(autoload 'epg-sub-key-validity "epg")
(autoload 'epg-sub-key-fingerprint "epg")
(autoload 'epg-sign-string "epg")
(autoload 'epg-encrypt-string "epg")
(autoload 'epg-configuration "epg-config")
(autoload 'epg-expand-group "epg-config")

(defvar mml1991-epg-secret-key-id-list nil)

(defun mml1991-epg-passphrase-callback (context key-id ignore)
  (if (eq key-id 'SYM)
      (epg-passphrase-callback-function context key-id nil)
    (let* ((entry (assoc key-id epg-user-id-alist))
	   (passphrase
	    (password-read
	     (format "GnuPG passphrase for %s: "
		     (if entry
			 (cdr entry)
		       key-id))
	     (if (eq key-id 'PIN)
		 "PIN"
	       key-id))))
      (when passphrase
	(let ((password-cache-expiry mml1991-passphrase-cache-expiry))
	  (password-cache-add key-id passphrase))
	(setq mml1991-epg-secret-key-id-list
	      (cons key-id mml1991-epg-secret-key-id-list))
	(copy-sequence passphrase)))))

(defun mml1991-epg-find-usable-key (keys usage)
  (catch 'found
    (while keys
      (let ((pointer (epg-key-sub-key-list (car keys))))
	;; The primary key will be marked as disabled, when the entire
	;; key is disabled (see 12 Field, Format of colon listings, in
	;; gnupg/doc/DETAILS)
	(unless (memq 'disabled (epg-sub-key-capability (car pointer)))
	  (while pointer
	    (if (and (memq usage (epg-sub-key-capability (car pointer)))
		     (not (memq (epg-sub-key-validity (car pointer))
				'(revoked expired))))
		(throw 'found (car keys)))
	    (setq pointer (cdr pointer)))))
      (setq keys (cdr keys)))))

;; XXX: since gpg --list-secret-keys does not return validity of each
;; key, `mml1991-epg-find-usable-key' defined above is not enough for
;; secret keys.  The function `mml1991-epg-find-usable-secret-key'
;; below looks at appropriate public keys to check usability.
(defun mml1991-epg-find-usable-secret-key (context name usage)
  (let ((secret-keys (epg-list-keys context name t))
	secret-key)
    (while (and (not secret-key) secret-keys)
      (if (mml1991-epg-find-usable-key
	   (epg-list-keys context (epg-sub-key-fingerprint
				   (car (epg-key-sub-key-list
					 (car secret-keys)))))
	   usage)
	  (setq secret-key (car secret-keys)
		secret-keys nil)
	(setq secret-keys (cdr secret-keys))))
    secret-key))

(defun mml1991-epg-sign (cont)
  (let ((context (epg-make-context))
	headers cte signer-key signers signature)
    (if (eq mm-sign-option 'guided)
	(setq signers (epa-select-keys context "Select keys for signing.
If no one is selected, default secret key is used.  "
				       mml1991-signers t))
      (if mml1991-signers
	  (setq signers (delq nil
			      (mapcar
			       (lambda (name)
				 (setq signer-key
				       (mml1991-epg-find-usable-secret-key
					context name 'sign))
				 (unless (or signer-key
					     (y-or-n-p
					      (format
					       "No secret key for %s; skip it? "
					       name)))
				   (error "No secret key for %s" name))
				 signer-key)
			       mml1991-signers)))))
    (epg-context-set-armor context t)
    (epg-context-set-textmode context t)
    (epg-context-set-signers context signers)
    (if mml1991-cache-passphrase
	(epg-context-set-passphrase-callback
	 context
	 #'mml1991-epg-passphrase-callback))
    ;; Don't sign headers.
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (setq headers (buffer-substring (point-min) (point)))
      (save-restriction
	(narrow-to-region (point-min) (point))
	(setq cte (mail-fetch-field "content-transfer-encoding")))
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(setq cte (intern (downcase cte)))
	(mm-decode-content-transfer-encoding cte)))
    (condition-case error
	(setq signature (epg-sign-string context (buffer-string) 'clear)
	      mml1991-epg-secret-key-id-list nil)
      (error
       (while mml1991-epg-secret-key-id-list
	 (password-cache-remove (car mml1991-epg-secret-key-id-list))
	 (setq mml1991-epg-secret-key-id-list
	       (cdr mml1991-epg-secret-key-id-list)))
       (signal (car error) (cdr error))))
    (delete-region (point-min) (point-max))
    (mm-with-unibyte-current-buffer
      (insert signature)
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (when cte
	(mm-encode-content-transfer-encoding cte))
      (goto-char (point-min))
      (when headers
	(insert headers))
      (insert "\n"))
    t))

(defun mml1991-epg-encrypt (cont &optional sign)
  (goto-char (point-min))
  (when (re-search-forward "^$" nil t)
    (let ((cte (save-restriction
		 (narrow-to-region (point-min) (point))
		 (mail-fetch-field "content-transfer-encoding"))))
      ;; Strip MIME headers since it will be ASCII armored.
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(mm-decode-content-transfer-encoding (intern (downcase cte))))))
  (let ((context (epg-make-context))
	(recipients
	 (if (message-options-get 'message-recipients)
	     (split-string
	      (message-options-get 'message-recipients)
	      "[ \f\t\n\r\v,]+")))
	recipient-key signer-key cipher signers config)
    (when mml1991-encrypt-to-self
      (unless mml1991-signers
	(error "mml1991-signers is not set"))
      (setq recipients (nconc recipients mml1991-signers)))
    ;; We should remove this check if epg-0.0.6 is released.
    (if (and (condition-case nil
		 (require 'epg-config)
	       (error))
	     (functionp #'epg-expand-group))
	(setq config (epg-configuration)
	      recipients
	      (apply #'nconc
		     (mapcar (lambda (recipient)
			       (or (epg-expand-group config recipient)
				   (list recipient)))
			     recipients))))
    (if (eq mm-encrypt-option 'guided)
	(setq recipients
	      (epa-select-keys context "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
			       recipients))
      (setq recipients
	    (delq nil (mapcar
		       (lambda (name)
			 (setq recipient-key (mml1991-epg-find-usable-key
					      (epg-list-keys context name)
					      'encrypt))
			 (unless (or recipient-key
				   (y-or-n-p
				    (format "No public key for %s; skip it? "
					    name)))
			   (error "No public key for %s" name))
			 recipient-key)
		       recipients)))
      (unless recipients
	(error "No recipient specified")))
    (when sign
      (if (eq mm-sign-option 'guided)
	  (setq signers (epa-select-keys context "Select keys for signing.
If no one is selected, default secret key is used.  "
					 mml1991-signers t))
	(if mml1991-signers
	    (setq signers (delq nil
				(mapcar
				 (lambda (name)
				   (mml1991-epg-find-usable-secret-key
				    context name 'sign))
				 mml1991-signers)))))
      (epg-context-set-signers context signers))
    (epg-context-set-armor context t)
    (epg-context-set-textmode context t)
    (if mml1991-cache-passphrase
	(epg-context-set-passphrase-callback
	 context
	 #'mml1991-epg-passphrase-callback))
    (condition-case error
	(setq cipher
	      (epg-encrypt-string context (buffer-string) recipients sign)
	      mml1991-epg-secret-key-id-list nil)
      (error
       (while mml1991-epg-secret-key-id-list
	 (password-cache-remove (car mml1991-epg-secret-key-id-list))
	 (setq mml1991-epg-secret-key-id-list
	       (cdr mml1991-epg-secret-key-id-list)))
       (signal (car error) (cdr error))))
    (delete-region (point-min) (point-max))
    (insert "\n" cipher))
  t)

;;;###autoload
(defun mml1991-encrypt (cont &optional sign)
  (let ((func (nth 2 (assq mml1991-use mml1991-function-alist))))
    (if func
	(funcall func cont sign)
      (error "Cannot find encrypt function"))))

;;;###autoload
(defun mml1991-sign (cont)
  (let ((func (nth 1 (assq mml1991-use mml1991-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find sign function"))))

(provide 'mml1991)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; mml1991.el ends here
