;;; org-crypt.el --- Public key encryption for org-mode entries

;; Copyright (C) 2007, 2009-2012  Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: org-crypt.el
;; Keywords: org-mode
;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Peter Jones <pjones@pmade.com>
;; Description: Adds public key encryption to org-mode buffers
;; URL: http://www.newartisans.com/software/emacs.html
;; Compatibility: Emacs22

;; This file is part of GNU Emacs.
;;
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

;; Right now this is just a set of functions to play with.  It depends
;; on the epg library.  Here's how you would use it:
;;
;; 1. To mark an entry for encryption, tag the heading with "crypt".
;;    You can change the tag to any complex tag matching string by
;;    setting the `org-crypt-tag-matcher' variable.
;;
;; 2. Set the encryption key to use in the `org-crypt-key' variable,
;;    or use `M-x org-set-property' to set the property CRYPTKEY to
;;    any address in your public keyring.  The text of the entry (but
;;    not its properties or headline) will be encrypted for this user.
;;    For them to read it, the corresponding secret key must be
;;    located in the secret key ring of the account where you try to
;;    decrypt it.  This makes it possible to leave secure notes that
;;    only the intended recipient can read in a shared-org-mode-files
;;    scenario.
;;    If the key is not set, org-crypt will default to symmetric encryption.
;;
;; 3. To later decrypt an entry, use `org-decrypt-entries' or
;;    `org-decrypt-entry'.  It might be useful to bind this to a key,
;;    like C-c C-/.  I hope that in the future, C-c C-r can be might
;;    overloaded to also decrypt an entry if it's encrypted, since
;;    that fits nicely with the meaning of "reveal".
;;
;; 4. To automatically encrypt all necessary entries when saving a
;;    file, call `org-crypt-use-before-save-magic' after loading
;;    org-crypt.el.

;;; Thanks:

;; - Carsten Dominik
;; - Vitaly Ostanin

(require 'org)

;;; Code:

(declare-function epg-decrypt-string "epg" (context cipher))
(declare-function epg-list-keys "epg" (context &optional name mode))
(declare-function epg-make-context "epg"
		  (&optional protocol armor textmode include-certs
			     cipher-algorithm digest-algorithm
			     compress-algorithm))
(declare-function epg-encrypt-string "epg"
		  (context plain recipients &optional sign always-trust))

(defgroup org-crypt nil
  "Org Crypt"
  :tag "Org Crypt"
  :group 'org)

(defcustom org-crypt-tag-matcher "crypt"
  "The tag matcher used to find headings whose contents should be encrypted.

See the \"Match syntax\" section of the org manual for more details."
  :type 'string
  :group 'org-crypt)

(defcustom org-crypt-key ""
  "The default key to use when encrypting the contents of a heading.

This setting can also be overridden in the CRYPTKEY property."
  :type 'string
  :group 'org-crypt)

(defcustom org-crypt-disable-auto-save 'ask
  "What org-decrypt should do if `auto-save-mode' is enabled.

t        : Disable auto-save-mode for the current buffer
           prior to decrypting an entry.

nil      : Leave auto-save-mode enabled.
           This may cause data to be written to disk unencrypted!

'ask     : Ask user whether or not to disable auto-save-mode
           for the current buffer.

'encrypt : Leave auto-save-mode enabled for the current buffer,
           but automatically re-encrypt all decrypted entries
           *before* auto-saving.
           NOTE: This only works for entries which have a tag
           that matches `org-crypt-tag-matcher'."
  :group 'org-crypt
  :type '(choice (const :tag "Always"  t)
                 (const :tag "Never"   nil)
                 (const :tag "Ask"     ask)
                 (const :tag "Encrypt" encrypt)))

(defun org-crypt-check-auto-save ()
  "Check whether auto-save-mode is enabled for the current buffer.

`auto-save-mode' may cause leakage when decrypting entries, so
check whether it's enabled, and decide what to do about it.

See `org-crypt-disable-auto-save'."
  (when buffer-auto-save-file-name
    (cond
     ((or
       (eq org-crypt-disable-auto-save t)
       (and
	(eq org-crypt-disable-auto-save 'ask)
	(y-or-n-p "org-decrypt: auto-save-mode may cause leakage. Disable it for current buffer? ")))
      (message (concat "org-decrypt: Disabling auto-save-mode for " (or (buffer-file-name) (current-buffer))))
      ; The argument to auto-save-mode has to be "-1", since
      ; giving a "nil" argument toggles instead of disabling.
      (auto-save-mode -1))
     ((eq org-crypt-disable-auto-save nil)
      (message "org-decrypt: Decrypting entry with auto-save-mode enabled. This may cause leakage."))
     ((eq org-crypt-disable-auto-save 'encrypt)
      (message "org-decrypt: Enabling re-encryption on auto-save.")
      (add-hook 'auto-save-hook
		(lambda ()
		  (message "org-crypt: Re-encrypting all decrypted entries due to auto-save.")
		  (org-encrypt-entries))
		nil t))
     (t nil))))

(defun org-crypt-key-for-heading ()
  "Return the encryption key for the current heading."
  (save-excursion
    (org-back-to-heading t)
    (or (org-entry-get nil "CRYPTKEY" 'selective)
        org-crypt-key
        (and (boundp 'epa-file-encrypt-to) epa-file-encrypt-to)
        (message "No crypt key set, using symmetric encryption."))))

(defun org-encrypt-string (str crypt-key)
  "Return STR encrypted with CRYPT-KEY."
  ;; Text and key have to be identical, otherwise we re-crypt.
  (if (and (string= crypt-key (get-text-property 0 'org-crypt-key str))
	   (string= (sha1 str) (get-text-property 0 'org-crypt-checksum str)))
      (get-text-property 0 'org-crypt-text str)
    (let ((epg-context (epg-make-context nil t t)))
      (epg-encrypt-string epg-context str (epg-list-keys epg-context crypt-key)))))

(defun org-encrypt-entry ()
  "Encrypt the content of the current headline."
  (interactive)
  (require 'epg)
  (save-excursion
    (org-back-to-heading t)
    (let ((start-heading (point)))
      (forward-line)
      (when (not (looking-at "-----BEGIN PGP MESSAGE-----"))
        (let ((folded (outline-invisible-p))
              (epg-context (epg-make-context nil t t))
              (crypt-key (org-crypt-key-for-heading))
              (beg (point))
              end encrypted-text)
          (goto-char start-heading)
          (org-end-of-subtree t t)
          (org-back-over-empty-lines)
          (setq end (point)
                encrypted-text
		(org-encrypt-string (buffer-substring beg end) crypt-key))
          (delete-region beg end)
          (insert encrypted-text)
          (when folded
            (goto-char start-heading)
            (hide-subtree))
          nil)))))

(defun org-decrypt-entry ()
  "Decrypt the content of the current headline."
  (interactive)
  (require 'epg)
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((heading-point (point))
	    (heading-was-invisible-p
	     (save-excursion
	       (outline-end-of-heading)
	       (outline-invisible-p))))
	(forward-line)
	(when (looking-at "-----BEGIN PGP MESSAGE-----")
	  (org-crypt-check-auto-save)
	  (let* ((end (save-excursion
			(search-forward "-----END PGP MESSAGE-----")
			(forward-line)
			(point)))
		 (epg-context (epg-make-context nil t t))
		 (encrypted-text (buffer-substring-no-properties (point) end))
		 (decrypted-text
		  (decode-coding-string
		   (epg-decrypt-string
		    epg-context
		    encrypted-text)
		   'utf-8)))
	    ;; Delete region starting just before point, because the
	    ;; outline property starts at the \n of the heading.
	    (delete-region (1- (point)) end)
	    ;; Store a checksum of the decrypted and the encrypted
	    ;; text value. This allow to reuse the same encrypted text
	    ;; if the text does not change, and therefore avoid a
	    ;; re-encryption process.
	    (insert "\n" (propertize decrypted-text
				     'org-crypt-checksum (sha1 decrypted-text)
				     'org-crypt-key (org-crypt-key-for-heading)
				     'org-crypt-text encrypted-text))
	    (when heading-was-invisible-p
	      (goto-char heading-point)
	      (org-flag-subtree t))
	    nil))))))

(defun org-encrypt-entries ()
  "Encrypt all top-level entries in the current buffer."
  (interactive)
  (let (todo-only)
    (org-scan-tags
     'org-encrypt-entry
     (cdr (org-make-tags-matcher org-crypt-tag-matcher))
     todo-only)))

(defun org-decrypt-entries ()
  "Decrypt all entries in the current buffer."
  (interactive)
  (let (todo-only)
    (org-scan-tags
     'org-decrypt-entry
     (cdr (org-make-tags-matcher org-crypt-tag-matcher))
     todo-only)))

(defun org-crypt-use-before-save-magic ()
  "Add a hook to automatically encrypt entries before a file is saved to disk."
  (add-hook
   'org-mode-hook
   (lambda () (add-hook 'before-save-hook 'org-encrypt-entries nil t))))

(add-hook 'org-reveal-start-hook 'org-decrypt-entry)

(provide 'org-crypt)

;;; org-crypt.el ends here
