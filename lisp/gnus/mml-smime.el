;;; mml-smime.el --- S/MIME support for MML

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: Gnus, MIME, S/MIME, MML

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

(eval-when-compile (require 'cl))

(require 'smime)
(require 'mm-decode)
(require 'mml-sec)
(autoload 'message-narrow-to-headers "message")
(autoload 'message-fetch-field "message")

(defcustom mml-smime-use (if (featurep 'epg) 'epg 'openssl)
  "Whether to use OpenSSL or EPG to decrypt S/MIME messages.
Defaults to EPG if it's loaded."
  :group 'mime-security
  :type '(choice (const :tag "EPG" epg)
                 (const :tag "OpenSSL" openssl)))

(defvar mml-smime-function-alist
  '((openssl mml-smime-openssl-sign
	     mml-smime-openssl-encrypt
	     mml-smime-openssl-sign-query
	     mml-smime-openssl-encrypt-query
	     mml-smime-openssl-verify
	     mml-smime-openssl-verify-test)
    (epg mml-smime-epg-sign
	 mml-smime-epg-encrypt
	 nil
	 nil
	 mml-smime-epg-verify
	 mml-smime-epg-verify-test)))

(defcustom mml-smime-cache-passphrase mml-secure-cache-passphrase
  "If t, cache passphrase."
  :group 'mime-security
  :type 'boolean)

(defcustom mml-smime-passphrase-cache-expiry mml-secure-passphrase-cache-expiry
  "How many seconds the passphrase is cached.
Whether the passphrase is cached at all is controlled by
`mml-smime-cache-passphrase'."
  :group 'mime-security
  :type 'integer)

(defcustom mml-smime-signers nil
  "A list of your own key ID which will be used to sign a message."
  :group 'mime-security
  :type '(repeat (string :tag "Key ID")))

(defun mml-smime-sign (cont)
  (let ((func (nth 1 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find sign function"))))

(defun mml-smime-encrypt (cont)
  (let ((func (nth 2 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find encrypt function"))))

(defun mml-smime-sign-query ()
  (let ((func (nth 3 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func))))

(defun mml-smime-encrypt-query ()
  (let ((func (nth 4 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func))))

(defun mml-smime-verify (handle ctl)
  (let ((func (nth 5 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func handle ctl)
      handle)))

(defun mml-smime-verify-test (handle ctl)
  (let ((func (nth 6 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func handle ctl))))

(defun mml-smime-openssl-sign (cont)
  (when (null smime-keys)
    (customize-variable 'smime-keys)
    (error "No S/MIME keys configured, use customize to add your key"))
  (smime-sign-buffer (cdr (assq 'keyfile cont)))
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n" t t))
  (goto-char (point-max)))

(defun mml-smime-openssl-encrypt (cont)
  (let (certnames certfiles tmp file tmpfiles)
    ;; xxx tmp files are always an security issue
    (while (setq tmp (pop cont))
      (if (and (consp tmp) (eq (car tmp) 'certfile))
	  (push (cdr tmp) certnames)))
    (while (setq tmp (pop certnames))
      (if (not (and (not (file-exists-p tmp))
		    (get-buffer tmp)))
	  (push tmp certfiles)
	(setq file (mm-make-temp-file (expand-file-name "mml."
							mm-tmp-directory)))
	(with-current-buffer tmp
	  (write-region (point-min) (point-max) file))
	(push file certfiles)
	(push file tmpfiles)))
    (if (smime-encrypt-buffer certfiles)
	(progn
	  (while (setq tmp (pop tmpfiles))
	    (delete-file tmp))
	  t)
      (while (setq tmp (pop tmpfiles))
	(delete-file tmp))
      nil))
  (goto-char (point-max)))

(defvar gnus-extract-address-components)

(defun mml-smime-openssl-sign-query ()
  ;; query information (what certificate) from user when MML tag is
  ;; added, for use later by the signing process
  (when (null smime-keys)
    (customize-variable 'smime-keys)
    (error "No S/MIME keys configured, use customize to add your key"))
  (list 'keyfile
	(if (= (length smime-keys) 1)
	    (cadar smime-keys)
	  (or (let ((from (cadr (funcall (if (boundp
					      'gnus-extract-address-components)
					     gnus-extract-address-components
					   'mail-extract-address-components)
					 (or (save-excursion
					       (save-restriction
						 (message-narrow-to-headers)
						 (message-fetch-field "from")))
					     "")))))
		(and from (smime-get-key-by-email from)))
	      (smime-get-key-by-email
	       (gnus-completing-read "Sign this part with what signature"
                                     (mapcar 'car smime-keys) nil nil nil
                                     (and (listp (car-safe smime-keys))
                                          (caar smime-keys))))))))

(defun mml-smime-get-file-cert ()
  (ignore-errors
    (list 'certfile (read-file-name
		     "File with recipient's S/MIME certificate: "
		     smime-certificate-directory nil t ""))))

(defun mml-smime-get-dns-cert ()
  ;; todo: deal with comma separated multiple recipients
  (let (result who bad cert)
    (condition-case ()
	(while (not result)
	  (setq who (read-from-minibuffer
		     (format "%sLookup certificate for: " (or bad ""))
		     (cadr (funcall (if (boundp
					 'gnus-extract-address-components)
					gnus-extract-address-components
				      'mail-extract-address-components)
				    (or (save-excursion
					  (save-restriction
					    (message-narrow-to-headers)
					    (message-fetch-field "to")))
					"")))))
	  (if (setq cert (smime-cert-by-dns who))
	      (setq result (list 'certfile (buffer-name cert)))
	    (setq bad (format "`%s' not found. " who))))
      (quit))
    result))

(defun mml-smime-get-ldap-cert ()
  ;; todo: deal with comma separated multiple recipients
  (let (result who bad cert)
    (condition-case ()
	(while (not result)
	  (setq who (read-from-minibuffer
		     (format "%sLookup certificate for: " (or bad ""))
		     (cadr (funcall gnus-extract-address-components
				    (or (save-excursion
					  (save-restriction
					    (message-narrow-to-headers)
					    (message-fetch-field "to")))
					"")))))
	  (if (setq cert (smime-cert-by-ldap who))
	      (setq result (list 'certfile (buffer-name cert)))
	    (setq bad (format "`%s' not found. " who))))
      (quit))
    result))

(autoload 'gnus-completing-read "gnus-util")

(defun mml-smime-openssl-encrypt-query ()
  ;; todo: try dns/ldap automatically first, before prompting user
  (let (certs done)
    (while (not done)
      (ecase (read (gnus-completing-read
		    "Fetch certificate from"
		    '("dns" "ldap" "file") t nil nil
                    "ldap"))
	(dns (setq certs (append certs
				 (mml-smime-get-dns-cert))))
	(ldap (setq certs (append certs
				  (mml-smime-get-ldap-cert))))
	(file (setq certs (append certs
				  (mml-smime-get-file-cert)))))
      (setq done (not (y-or-n-p "Add more recipients? "))))
    certs))

(defun mml-smime-openssl-verify (handle ctl)
  (with-temp-buffer
    (insert-buffer-substring (mm-handle-multipart-original-buffer ctl))
    (goto-char (point-min))
    (insert (format "Content-Type: %s; " (mm-handle-media-type ctl)))
    (insert (format "protocol=\"%s\"; "
		    (mm-handle-multipart-ctl-parameter ctl 'protocol)))
    (insert (format "micalg=\"%s\"; "
		    (mm-handle-multipart-ctl-parameter ctl 'micalg)))
    (insert (format "boundary=\"%s\"\n\n"
		    (mm-handle-multipart-ctl-parameter ctl 'boundary)))
    (when (get-buffer smime-details-buffer)
      (kill-buffer smime-details-buffer))
    (let ((buf (current-buffer))
	  (good-signature (smime-noverify-buffer))
	  (good-certificate (and (or smime-CA-file smime-CA-directory)
				 (smime-verify-buffer)))
	  addresses openssl-output)
      (setq openssl-output (with-current-buffer smime-details-buffer
			     (buffer-string)))
      (if (not good-signature)
	  (progn
	    ;; we couldn't verify message, fail with openssl output as message
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Failed")
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (concat "OpenSSL failed to verify message integrity:\n"
		     "-------------------------------------------\n"
		     openssl-output)))
	;; verify mail addresses in mail against those in certificate
	(when (and (smime-pkcs7-region (point-min) (point-max))
		   (smime-pkcs7-certificates-region (point-min) (point-max)))
	  (with-temp-buffer
	    (insert-buffer-substring buf)
	    (goto-char (point-min))
	    (while (re-search-forward "-----END CERTIFICATE-----" nil t)
	      (when (smime-pkcs7-email-region (point-min) (point))
		(setq addresses (append (smime-buffer-as-string-region
					 (point-min) (point)) addresses)))
	      (delete-region (point-min) (point)))
	    (setq addresses (mapcar 'downcase addresses))))
	(if (not (member (downcase (or (mm-handle-multipart-from ctl) "")) addresses))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Sender address forged")
	  (if good-certificate
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-info "Ok (sender authenticated)")
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Ok (sender not trusted)")))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-details
	 (concat "Sender claimed to be: " (mm-handle-multipart-from ctl) "\n"
		 (if addresses
		     (concat "Addresses in certificate: "
			     (mapconcat 'identity addresses ", "))
		   "No addresses found in certificate. (Requires OpenSSL 0.9.6 or later.)")
		 "\n" "\n"
		 "OpenSSL output:\n"
		 "---------------\n" openssl-output "\n"
		 "Certificate(s) inside S/MIME signature:\n"
		 "---------------------------------------\n"
		 (buffer-string) "\n")))))
  handle)

(defun mml-smime-openssl-verify-test (handle ctl)
  smime-openssl-program)

(defvar epg-user-id-alist)
(defvar epg-digest-algorithm-alist)
(defvar inhibit-redisplay)
(defvar password-cache-expiry)

(eval-when-compile
  (autoload 'epg-make-context "epg")
  (autoload 'epg-context-set-armor "epg")
  (autoload 'epg-context-set-signers "epg")
  (autoload 'epg-context-result-for "epg")
  (autoload 'epg-new-signature-digest-algorithm "epg")
  (autoload 'epg-verify-result-to-string "epg")
  (autoload 'epg-list-keys "epg")
  (autoload 'epg-decrypt-string "epg")
  (autoload 'epg-verify-string "epg")
  (autoload 'epg-sign-string "epg")
  (autoload 'epg-encrypt-string "epg")
  (autoload 'epg-passphrase-callback-function "epg")
  (autoload 'epg-context-set-passphrase-callback "epg")
  (autoload 'epg-configuration "epg-config")
  (autoload 'epg-expand-group "epg-config")
  (autoload 'epa-select-keys "epa"))

(defvar mml-smime-epg-secret-key-id-list nil)

(defun mml-smime-epg-passphrase-callback (context key-id ignore)
  (if (eq key-id 'SYM)
      (epg-passphrase-callback-function context key-id nil)
    (let* (entry
	   (passphrase
	    (password-read
	     (if (eq key-id 'PIN)
		 "Passphrase for PIN: "
	       (if (setq entry (assoc key-id epg-user-id-alist))
		   (format "Passphrase for %s %s: " key-id (cdr entry))
		 (format "Passphrase for %s: " key-id)))
	     (if (eq key-id 'PIN)
		 "PIN"
	       key-id))))
      (when passphrase
	(let ((password-cache-expiry mml-smime-passphrase-cache-expiry))
	  (password-cache-add key-id passphrase))
	(setq mml-smime-epg-secret-key-id-list
	      (cons key-id mml-smime-epg-secret-key-id-list))
	(copy-sequence passphrase)))))

(declare-function epg-key-sub-key-list   "ext:epg" (key))
(declare-function epg-sub-key-capability "ext:epg" (sub-key))
(declare-function epg-sub-key-validity   "ext:epg" (sub-key))

(defun mml-smime-epg-find-usable-key (keys usage)
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

(autoload 'mml-compute-boundary "mml")

;; We require mm-decode, which requires mm-bodies, which autoloads
;; message-options-get (!).
(declare-function message-options-set "message" (symbol value))

(defun mml-smime-epg-sign (cont)
  (let* ((inhibit-redisplay t)
	 (context (epg-make-context 'CMS))
	 (boundary (mml-compute-boundary cont))
	 signer-key
	 (signers
	  (or (message-options-get 'mml-smime-epg-signers)
	      (message-options-set
	      'mml-smime-epg-signers
	      (if (eq mm-sign-option 'guided)
		  (epa-select-keys context "\
Select keys for signing.
If no one is selected, default secret key is used.  "
				   mml-smime-signers t)
		(if mml-smime-signers
		    (mapcar
		     (lambda (signer)
		       (setq signer-key (mml-smime-epg-find-usable-key
					 (epg-list-keys context signer t)
					 'sign))
		       (unless (or signer-key
				   (y-or-n-p
				    (format "No secret key for %s; skip it? "
					    signer)))
			 (error "No secret key for %s" signer))
		       signer-key)
		     mml-smime-signers))))))
	 signature micalg)
    (epg-context-set-signers context signers)
    (if mml-smime-cache-passphrase
	(epg-context-set-passphrase-callback
	 context
	 #'mml-smime-epg-passphrase-callback))
    (condition-case error
	(setq signature (epg-sign-string context
					 (mm-replace-in-string (buffer-string)
							       "\n" "\r\n")
					 t)
	      mml-smime-epg-secret-key-id-list nil)
      (error
       (while mml-smime-epg-secret-key-id-list
	 (password-cache-remove (car mml-smime-epg-secret-key-id-list))
	 (setq mml-smime-epg-secret-key-id-list
	       (cdr mml-smime-epg-secret-key-id-list)))
       (signal (car error) (cdr error))))
    (if (epg-context-result-for context 'sign)
	(setq micalg (epg-new-signature-digest-algorithm
		      (car (epg-context-result-for context 'sign)))))
    (goto-char (point-min))
    (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		    boundary))
    (if micalg
	(insert (format "\tmicalg=%s; "
			(downcase
			 (cdr (assq micalg
				    epg-digest-algorithm-alist))))))
    (insert "protocol=\"application/pkcs7-signature\"\n")
    (insert (format "\n--%s\n" boundary))
    (goto-char (point-max))
    (insert (format "\n--%s\n" boundary))
    (insert "Content-Type: application/pkcs7-signature; name=smime.p7s
Content-Transfer-Encoding: base64
Content-Disposition: attachment; filename=smime.p7s

")
    (insert (base64-encode-string signature) "\n")
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

(defun mml-smime-epg-encrypt (cont)
  (let ((inhibit-redisplay t)
	(context (epg-make-context 'CMS))
	(config (epg-configuration))
	(recipients (message-options-get 'mml-smime-epg-recipients))
	cipher signers
	(boundary (mml-compute-boundary cont))
	recipient-key)
    (unless recipients
      (setq recipients
	    (apply #'nconc
		   (mapcar
		    (lambda (recipient)
		      (or (epg-expand-group config recipient)
			  (list recipient)))
		    (split-string
		     (or (message-options-get 'message-recipients)
			 (message-options-set 'message-recipients
					      (read-string "Recipients: ")))
		     "[ \f\t\n\r\v,]+"))))
      (if (eq mm-encrypt-option 'guided)
	  (setq recipients
		(epa-select-keys context "\
Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
				 recipients))
	(setq recipients
	      (mapcar
	       (lambda (recipient)
		 (setq recipient-key (mml-smime-epg-find-usable-key
				      (epg-list-keys context recipient)
				      'encrypt))
		 (unless (or recipient-key
			     (y-or-n-p
			      (format "No public key for %s; skip it? "
				      recipient)))
		   (error "No public key for %s" recipient))
		 recipient-key)
	       recipients))
	(unless recipients
	  (error "No recipient specified")))
      (message-options-set 'mml-smime-epg-recipients recipients))
    (if mml-smime-cache-passphrase
	(epg-context-set-passphrase-callback
	 context
	 #'mml-smime-epg-passphrase-callback))
    (condition-case error
	(setq cipher
	      (epg-encrypt-string context (buffer-string) recipients)
	      mml-smime-epg-secret-key-id-list nil)
      (error
       (while mml-smime-epg-secret-key-id-list
	 (password-cache-remove (car mml-smime-epg-secret-key-id-list))
	 (setq mml-smime-epg-secret-key-id-list
	       (cdr mml-smime-epg-secret-key-id-list)))
       (signal (car error) (cdr error))))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert "\
Content-Type: application/pkcs7-mime;
 smime-type=enveloped-data;
 name=smime.p7m
Content-Transfer-Encoding: base64
Content-Disposition: attachment; filename=smime.p7m

")
    (insert (base64-encode-string cipher))
    (goto-char (point-max))))

(defun mml-smime-epg-verify (handle ctl)
  (catch 'error
    (let ((inhibit-redisplay t)
	  context plain signature-file part signature)
      (when (or (null (setq part (mm-find-raw-part-by-type
				  ctl (or (mm-handle-multipart-ctl-parameter
					   ctl 'protocol)
					  "application/pkcs7-signature")
				  t)))
		(null (setq signature (or (mm-find-part-by-type
					   (cdr handle)
					   "application/pkcs7-signature"
					   nil t)
					  (mm-find-part-by-type
					   (cdr handle)
					   "application/x-pkcs7-signature"
					   nil t)))))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (setq part (mm-replace-in-string part "\n" "\r\n")
	    context (epg-make-context 'CMS))
      (condition-case error
	  (setq plain (epg-verify-string context (mm-get-part signature) part))
	(error
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-info "Failed")
	 (if (eq (car error) 'quit)
	     (mm-set-handle-multipart-parameter
	      mm-security-handle 'gnus-details "Quit.")
	   (mm-set-handle-multipart-parameter
	    mm-security-handle 'gnus-details (format "%S" error)))
	 (throw 'error handle)))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info
       (epg-verify-result-to-string (epg-context-result-for context 'verify)))
      handle)))

(defun mml-smime-epg-verify-test (handle ctl)
  t)

(provide 'mml-smime)

;;; mml-smime.el ends here
