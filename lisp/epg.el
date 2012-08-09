;;; epg.el --- the EasyPG Library -*- lexical-binding: t -*-
;; Copyright (C) 1999-2000, 2002-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG
;; Version: 1.0.0

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

(require 'epg-config)

(defvar epg-user-id nil
  "GnuPG ID of your default identity.")

(defvar epg-user-id-alist nil
  "An alist mapping from key ID to user ID.")

(defvar epg-last-status nil)
(defvar epg-read-point nil)
(defvar epg-process-filter-running nil)
(defvar epg-pending-status-list nil)
(defvar epg-key-id nil)
(defvar epg-context nil)
(defvar epg-debug-buffer nil)

;; from gnupg/include/cipher.h
(defconst epg-cipher-algorithm-alist
  '((0 . "NONE")
    (1 . "IDEA")
    (2 . "3DES")
    (3 . "CAST5")
    (4 . "BLOWFISH")
    (7 . "AES")
    (8 . "AES192")
    (9 . "AES256")
    (10 . "TWOFISH")
    (11 . "CAMELLIA128")
    (12 . "CAMELLIA256")
    (110 . "DUMMY")))

;; from gnupg/include/cipher.h
(defconst epg-pubkey-algorithm-alist
  '((1 . "RSA")
    (2 . "RSA_E")
    (3 . "RSA_S")
    (16 . "ELGAMAL_E")
    (17 . "DSA")
    (20 . "ELGAMAL")))

;; from gnupg/include/cipher.h
(defconst epg-digest-algorithm-alist
  '((1 . "MD5")
    (2 . "SHA1")
    (3 . "RIPEMD160")
    (8 . "SHA256")
    (9 . "SHA384")
    (10 . "SHA512")
    (11 . "SHA224")))

;; from gnupg/include/cipher.h
(defconst epg-compress-algorithm-alist
  '((0 . "NONE")
    (1 . "ZIP")
    (2 . "ZLIB")
    (3 . "BZIP2")))

(defconst epg-invalid-recipients-reason-alist
  '((0 . "No specific reason given")
    (1 . "Not Found")
    (2 . "Ambiguous specification")
    (3 . "Wrong key usage")
    (4 . "Key revoked")
    (5 . "Key expired")
    (6 . "No CRL known")
    (7 . "CRL too old")
    (8 . "Policy mismatch")
    (9 . "Not a secret key")
    (10 . "Key not trusted")))

(defconst epg-delete-problem-reason-alist
  '((1 . "No such key")
    (2 . "Must delete secret key first")
    (3 . "Ambiguous specification")))

(defconst epg-import-ok-reason-alist
  '((0 . "Not actually changed")
    (1 . "Entirely new key")
    (2 . "New user IDs")
    (4 . "New signatures")
    (8 . "New subkeys")
    (16 . "Contains private key")))

(defconst epg-import-problem-reason-alist
  '((0 . "No specific reason given")
    (1 . "Invalid Certificate")
    (2 . "Issuer Certificate missing")
    (3 . "Certificate Chain too long")
    (4 . "Error storing certificate")))

(defconst epg-no-data-reason-alist
  '((1 . "No armored data")
    (2 . "Expected a packet but did not found one")
    (3 . "Invalid packet found, this may indicate a non OpenPGP message")
    (4 . "Signature expected but not found")))

(defconst epg-unexpected-reason-alist nil)

(defvar epg-key-validity-alist
  '((?o . unknown)
    (?i . invalid)
    (?d . disabled)
    (?r . revoked)
    (?e . expired)
    (?- . none)
    (?q . undefined)
    (?n . never)
    (?m . marginal)
    (?f . full)
    (?u . ultimate)))

(defvar epg-key-capablity-alist
  '((?e . encrypt)
    (?s . sign)
    (?c . certify)
    (?a . authentication)
    (?D . disabled)))

(defvar epg-new-signature-type-alist
  '((?D . detached)
    (?C . clear)
    (?S . normal)))

(defvar epg-dn-type-alist
  '(("1.2.840.113549.1.9.1" . "EMail")
    ("2.5.4.12" . "T")
    ("2.5.4.42" . "GN")
    ("2.5.4.4" . "SN")
    ("0.2.262.1.10.7.20" . "NameDistinguisher")
    ("2.5.4.16" . "ADDR")
    ("2.5.4.15" . "BC")
    ("2.5.4.13" . "D")
    ("2.5.4.17" . "PostalCode")
    ("2.5.4.65" . "Pseudo")
    ("2.5.4.5" . "SerialNumber")))

(defvar epg-prompt-alist nil)

(put 'epg-error 'error-conditions '(epg-error error))

(defun epg-make-data-from-file (file)
  "Make a data object from FILE."
  (cons 'epg-data (vector file nil)))

(defun epg-make-data-from-string (string)
  "Make a data object from STRING."
  (cons 'epg-data (vector nil string)))

(defun epg-data-file (data)
  "Return the file of DATA."
  (unless (eq (car-safe data) 'epg-data)
    (signal 'wrong-type-argument (list 'epg-data-p data)))
  (aref (cdr data) 0))

(defun epg-data-string (data)
  "Return the string of DATA."
  (unless (eq (car-safe data) 'epg-data)
    (signal 'wrong-type-argument (list 'epg-data-p data)))
  (aref (cdr data) 1))

;;;###autoload
(defun epg-make-context (&optional protocol armor textmode include-certs
				   cipher-algorithm digest-algorithm
				   compress-algorithm)
  "Return a context object."
  (cons 'epg-context
	(vector (or protocol 'OpenPGP) armor textmode include-certs
		cipher-algorithm digest-algorithm compress-algorithm
		(list #'epg-passphrase-callback-function)
		nil
		nil nil nil nil nil nil)))

(defun epg-context-protocol (context)
  "Return the protocol used within CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 0))

(defun epg-context-armor (context)
  "Return t if the output should be ASCII armored in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 1))

(defun epg-context-textmode (context)
  "Return t if canonical text mode should be used in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 2))

(defun epg-context-include-certs (context)
  "Return how many certificates should be included in an S/MIME signed message."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 3))

(defun epg-context-cipher-algorithm (context)
  "Return the cipher algorithm in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 4))

(defun epg-context-digest-algorithm (context)
  "Return the digest algorithm in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 5))

(defun epg-context-compress-algorithm (context)
  "Return the compress algorithm in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 6))

(defun epg-context-passphrase-callback (context)
  "Return the function used to query passphrase."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 7))

(defun epg-context-progress-callback (context)
  "Return the function which handles progress update."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 8))

(defun epg-context-signers (context)
  "Return the list of key-id for signing."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 9))

(defun epg-context-sig-notations (context)
  "Return the list of notations for signing."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 10))

(defun epg-context-process (context)
  "Return the process object of `epg-gpg-program'.
This function is for internal use only."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 11))

(defun epg-context-output-file (context)
  "Return the output file of `epg-gpg-program'.
This function is for internal use only."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 12))

(defun epg-context-result (context)
  "Return the result of the previous cryptographic operation."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 13))

(defun epg-context-operation (context)
  "Return the name of the current cryptographic operation."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aref (cdr context) 14))

(defun epg-context-set-protocol (context protocol)
  "Set the protocol used within CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 0 protocol))

(defun epg-context-set-armor (context armor)
  "Specify if the output should be ASCII armored in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 1 armor))

(defun epg-context-set-textmode (context textmode)
  "Specify if canonical text mode should be used in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 2 textmode))

(defun epg-context-set-include-certs (context include-certs)
 "Set how many certificates should be included in an S/MIME signed message."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 3 include-certs))

(defun epg-context-set-cipher-algorithm (context cipher-algorithm)
 "Set the cipher algorithm in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 4 cipher-algorithm))

(defun epg-context-set-digest-algorithm (context digest-algorithm)
 "Set the digest algorithm in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 5 digest-algorithm))

(defun epg-context-set-compress-algorithm (context compress-algorithm)
 "Set the compress algorithm in CONTEXT."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 6 compress-algorithm))

(defun epg-context-set-passphrase-callback (context
					    passphrase-callback)
  "Set the function used to query passphrase.

PASSPHRASE-CALLBACK is either a function, or a cons-cell whose
car is a function and cdr is a callback data.

The function gets three arguments: the context, the key-id in
question, and the callback data (if any).

The callback may not be called if you use GnuPG 2.x, which relies
on the external program called `gpg-agent' for passphrase query.
If you really want to intercept passphrase query, consider
installing GnuPG 1.x _along with_ GnuPG 2.x, which does passphrase
query by itself and Emacs can intercept them."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 7 (if (consp passphrase-callback)
			    passphrase-callback
			  (list passphrase-callback))))

(defun epg-context-set-progress-callback (context
					  progress-callback)
  "Set the function which handles progress update.

PROGRESS-CALLBACK is either a function, or a cons-cell whose
car is a function and cdr is a callback data.

The function gets six arguments: the context, the operation
description, the character to display a progress unit, the
current amount done, the total amount to be done, and the
callback data (if any)."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 8 (if (consp progress-callback)
			    progress-callback
			  (list progress-callback))))

(defun epg-context-set-signers (context signers)
  "Set the list of key-id for signing."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 9 signers))

(defun epg-context-set-sig-notations (context notations)
  "Set the list of notations for signing."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 10 notations))

(defun epg-context-set-process (context process)
  "Set the process object of `epg-gpg-program'.
This function is for internal use only."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 11 process))

(defun epg-context-set-output-file (context output-file)
  "Set the output file of `epg-gpg-program'.
This function is for internal use only."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 12 output-file))

(defun epg-context-set-result (context result)
  "Set the result of the previous cryptographic operation."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 13 result))

(defun epg-context-set-operation (context operation)
  "Set the name of the current cryptographic operation."
  (unless (eq (car-safe context) 'epg-context)
    (signal 'wrong-type-argument (list 'epg-context-p context)))
  (aset (cdr context) 14 operation))

(defun epg-make-signature (status &optional key-id)
  "Return a signature object."
  (cons 'epg-signature (vector status key-id nil nil nil nil nil nil nil nil
			       nil)))

(defun epg-signature-status (signature)
  "Return the status code of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 0))

(defun epg-signature-key-id (signature)
  "Return the key-id of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 1))

(defun epg-signature-validity (signature)
  "Return the validity of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 2))

(defun epg-signature-fingerprint (signature)
  "Return the fingerprint of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 3))

(defun epg-signature-creation-time (signature)
  "Return the creation time of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 4))

(defun epg-signature-expiration-time (signature)
  "Return the expiration time of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 5))

(defun epg-signature-pubkey-algorithm (signature)
  "Return the public key algorithm of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 6))

(defun epg-signature-digest-algorithm (signature)
  "Return the digest algorithm of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 7))

(defun epg-signature-class (signature)
  "Return the class of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 8))

(defun epg-signature-version (signature)
  "Return the version of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 9))

(defun epg-sig-notations (signature)
  "Return the list of notations of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aref (cdr signature) 10))

(defun epg-signature-set-status (signature status)
 "Set the status code of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 0 status))

(defun epg-signature-set-key-id (signature key-id)
 "Set the key-id of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 1 key-id))

(defun epg-signature-set-validity (signature validity)
 "Set the validity of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 2 validity))

(defun epg-signature-set-fingerprint (signature fingerprint)
 "Set the fingerprint of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 3 fingerprint))

(defun epg-signature-set-creation-time (signature creation-time)
  "Set the creation time of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 4 creation-time))

(defun epg-signature-set-expiration-time (signature expiration-time)
  "Set the expiration time of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 5 expiration-time))

(defun epg-signature-set-pubkey-algorithm (signature pubkey-algorithm)
  "Set the public key algorithm of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 6 pubkey-algorithm))

(defun epg-signature-set-digest-algorithm (signature digest-algorithm)
  "Set the digest algorithm of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 7 digest-algorithm))

(defun epg-signature-set-class (signature class)
  "Set the class of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 8 class))

(defun epg-signature-set-version (signature version)
  "Set the version of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 9 version))

(defun epg-signature-set-notations (signature notations)
  "Set the list of notations of SIGNATURE."
  (unless (eq (car-safe signature) 'epg-signature)
    (signal 'wrong-type-argument (list 'epg-signature-p signature)))
  (aset (cdr signature) 10 notations))

(defun epg-make-new-signature (type pubkey-algorithm digest-algorithm
				    class creation-time fingerprint)
  "Return a new signature object."
  (cons 'epg-new-signature (vector type pubkey-algorithm digest-algorithm
				   class creation-time fingerprint)))

(defun epg-new-signature-type (new-signature)
  "Return the type of NEW-SIGNATURE."
  (unless (eq (car-safe new-signature) 'epg-new-signature)
    (signal 'wrong-type-argument (list 'epg-new-signature-p new-signature)))
  (aref (cdr new-signature) 0))

(defun epg-new-signature-pubkey-algorithm (new-signature)
  "Return the public key algorithm of NEW-SIGNATURE."
  (unless (eq (car-safe new-signature) 'epg-new-signature)
    (signal 'wrong-type-argument (list 'epg-new-signature-p new-signature)))
  (aref (cdr new-signature) 1))

(defun epg-new-signature-digest-algorithm (new-signature)
  "Return the digest algorithm of NEW-SIGNATURE."
  (unless (eq (car-safe new-signature) 'epg-new-signature)
    (signal 'wrong-type-argument (list 'epg-new-signature-p new-signature)))
  (aref (cdr new-signature) 2))

(defun epg-new-signature-class (new-signature)
  "Return the class of NEW-SIGNATURE."
  (unless (eq (car-safe new-signature) 'epg-new-signature)
    (signal 'wrong-type-argument (list 'epg-new-signature-p new-signature)))
  (aref (cdr new-signature) 3))

(defun epg-new-signature-creation-time (new-signature)
  "Return the creation time of NEW-SIGNATURE."
  (unless (eq (car-safe new-signature) 'epg-new-signature)
    (signal 'wrong-type-argument (list 'epg-new-signature-p new-signature)))
  (aref (cdr new-signature) 4))

(defun epg-new-signature-fingerprint (new-signature)
  "Return the fingerprint of NEW-SIGNATURE."
  (unless (eq (car-safe new-signature) 'epg-new-signature)
    (signal 'wrong-type-argument (list 'epg-new-signature-p new-signature)))
  (aref (cdr new-signature) 5))

(defun epg-make-key (owner-trust)
  "Return a key object."
  (cons 'epg-key (vector owner-trust nil nil)))

(defun epg-key-owner-trust (key)
  "Return the owner trust of KEY."
  (unless (eq (car-safe key) 'epg-key)
    (signal 'wrong-type-argument (list 'epg-key-p key)))
  (aref (cdr key) 0))

(defun epg-key-sub-key-list (key)
  "Return the sub key list of KEY."
  (unless (eq (car-safe key) 'epg-key)
    (signal 'wrong-type-argument (list 'epg-key-p key)))
  (aref (cdr key) 1))

(defun epg-key-user-id-list (key)
  "Return the user ID list of KEY."
  (unless (eq (car-safe key) 'epg-key)
    (signal 'wrong-type-argument (list 'epg-key-p key)))
  (aref (cdr key) 2))

(defun epg-key-set-sub-key-list (key sub-key-list)
  "Set the sub key list of KEY."
  (unless (eq (car-safe key) 'epg-key)
    (signal 'wrong-type-argument (list 'epg-key-p key)))
  (aset (cdr key) 1 sub-key-list))

(defun epg-key-set-user-id-list (key user-id-list)
  "Set the user ID list of KEY."
  (unless (eq (car-safe key) 'epg-key)
    (signal 'wrong-type-argument (list 'epg-key-p key)))
  (aset (cdr key) 2 user-id-list))

(defun epg-make-sub-key (validity capability secret-p algorithm length id
				  creation-time expiration-time)
  "Return a sub key object."
  (cons 'epg-sub-key
	(vector validity capability secret-p algorithm length id creation-time
		expiration-time nil)))

(defun epg-sub-key-validity (sub-key)
  "Return the validity of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 0))

(defun epg-sub-key-capability (sub-key)
  "Return the capability of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 1))

(defun epg-sub-key-secret-p (sub-key)
  "Return non-nil if SUB-KEY is a secret key."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 2))

(defun epg-sub-key-algorithm (sub-key)
  "Return the algorithm of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 3))

(defun epg-sub-key-length (sub-key)
  "Return the length of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 4))

(defun epg-sub-key-id (sub-key)
  "Return the ID of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 5))

(defun epg-sub-key-creation-time (sub-key)
  "Return the creation time of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 6))

(defun epg-sub-key-expiration-time (sub-key)
  "Return the expiration time of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 7))

(defun epg-sub-key-fingerprint (sub-key)
  "Return the fingerprint of SUB-KEY."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aref (cdr sub-key) 8))

(defun epg-sub-key-set-fingerprint (sub-key fingerprint)
  "Set the fingerprint of SUB-KEY.
This function is for internal use only."
  (unless (eq (car-safe sub-key) 'epg-sub-key)
    (signal 'wrong-type-argument (list 'epg-sub-key-p sub-key)))
  (aset (cdr sub-key) 8 fingerprint))

(defun epg-make-user-id (validity string)
  "Return a user ID object."
  (cons 'epg-user-id (vector validity string nil)))

(defun epg-user-id-validity (user-id)
  "Return the validity of USER-ID."
  (unless (eq (car-safe user-id) 'epg-user-id)
    (signal 'wrong-type-argument (list 'epg-user-id-p user-id)))
  (aref (cdr user-id) 0))

(defun epg-user-id-string (user-id)
  "Return the name of USER-ID."
  (unless (eq (car-safe user-id) 'epg-user-id)
    (signal 'wrong-type-argument (list 'epg-user-id-p user-id)))
  (aref (cdr user-id) 1))

(defun epg-user-id-signature-list (user-id)
  "Return the signature list of USER-ID."
  (unless (eq (car-safe user-id) 'epg-user-id)
    (signal 'wrong-type-argument (list 'epg-user-id-p user-id)))
  (aref (cdr user-id) 2))

(defun epg-user-id-set-signature-list (user-id signature-list)
  "Set the signature list of USER-ID."
  (unless (eq (car-safe user-id) 'epg-user-id)
    (signal 'wrong-type-argument (list 'epg-user-id-p user-id)))
  (aset (cdr user-id) 2 signature-list))

(defun epg-make-key-signature (validity pubkey-algorithm key-id creation-time
					expiration-time	user-id class
					exportable-p)
  "Return a key signature object."
  (cons 'epg-key-signature
	(vector validity pubkey-algorithm key-id creation-time expiration-time
		user-id class exportable-p)))

(defun epg-key-signature-validity (key-signature)
  "Return the validity of KEY-SIGNATURE."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 0))

(defun epg-key-signature-pubkey-algorithm (key-signature)
  "Return the public key algorithm of KEY-SIGNATURE."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 1))

(defun epg-key-signature-key-id (key-signature)
  "Return the key-id of KEY-SIGNATURE."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 2))

(defun epg-key-signature-creation-time (key-signature)
  "Return the creation time of KEY-SIGNATURE."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 3))

(defun epg-key-signature-expiration-time (key-signature)
  "Return the expiration time of KEY-SIGNATURE."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 4))

(defun epg-key-signature-user-id (key-signature)
  "Return the user-id of KEY-SIGNATURE."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 5))

(defun epg-key-signature-class (key-signature)
  "Return the class of KEY-SIGNATURE."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 6))

(defun epg-key-signature-exportable-p (key-signature)
  "Return t if KEY-SIGNATURE is exportable."
  (unless (eq (car-safe key-signature) 'epg-key-signature)
    (signal 'wrong-type-argument (list 'epg-key-signature-p key-signature)))
  (aref (cdr key-signature) 7))

(defun epg-make-sig-notation (name value &optional human-readable
					 critical)
  "Return a notation object."
  (cons 'epg-sig-notation (vector name value human-readable critical)))

(defun epg-sig-notation-name (sig-notation)
  "Return the name of SIG-NOTATION."
  (unless (eq (car-safe sig-notation) 'epg-sig-notation)
    (signal 'wrong-type-argument (list 'epg-sig-notation-p
				       sig-notation)))
  (aref (cdr sig-notation) 0))

(defun epg-sig-notation-value (sig-notation)
  "Return the value of SIG-NOTATION."
  (unless (eq (car-safe sig-notation) 'epg-sig-notation)
    (signal 'wrong-type-argument (list 'epg-sig-notation-p
				       sig-notation)))
  (aref (cdr sig-notation) 1))

(defun epg-sig-notation-human-readable (sig-notation)
  "Return the human-readable of SIG-NOTATION."
  (unless (eq (car-safe sig-notation) 'epg-sig-notation)
    (signal 'wrong-type-argument (list 'epg-sig-notation-p
				       sig-notation)))
  (aref (cdr sig-notation) 2))

(defun epg-sig-notation-critical (sig-notation)
  "Return the critical of SIG-NOTATION."
  (unless (eq (car-safe sig-notation) 'epg-sig-notation)
    (signal 'wrong-type-argument (list 'epg-sig-notation-p
				       sig-notation)))
  (aref (cdr sig-notation) 3))

(defun epg-sig-notation-set-value (sig-notation value)
  "Set the value of SIG-NOTATION."
  (unless (eq (car-safe sig-notation) 'epg-sig-notation)
    (signal 'wrong-type-argument (list 'epg-sig-notation-p
				       sig-notation)))
  (aset (cdr sig-notation) 1 value))

(defun epg-make-import-status (fingerprint &optional reason new user-id
					   signature sub-key secret)
  "Return an import status object."
  (cons 'epg-import-status (vector fingerprint reason new user-id signature
				   sub-key secret)))

(defun epg-import-status-fingerprint (import-status)
  "Return the fingerprint of the key that was considered."
  (unless (eq (car-safe import-status) 'epg-import-status)
    (signal 'wrong-type-argument (list 'epg-import-status-p import-status)))
  (aref (cdr import-status) 0))

(defun epg-import-status-reason (import-status)
  "Return the reason code for import failure."
  (unless (eq (car-safe import-status) 'epg-import-status)
    (signal 'wrong-type-argument (list 'epg-import-status-p import-status)))
  (aref (cdr import-status) 1))

(defun epg-import-status-new (import-status)
  "Return t if the imported key was new."
  (unless (eq (car-safe import-status) 'epg-import-status)
    (signal 'wrong-type-argument (list 'epg-import-status-p import-status)))
  (aref (cdr import-status) 2))

(defun epg-import-status-user-id (import-status)
  "Return t if the imported key contained new user IDs."
  (unless (eq (car-safe import-status) 'epg-import-status)
    (signal 'wrong-type-argument (list 'epg-import-status-p import-status)))
  (aref (cdr import-status) 3))

(defun epg-import-status-signature (import-status)
  "Return t if the imported key contained new signatures."
  (unless (eq (car-safe import-status) 'epg-import-status)
    (signal 'wrong-type-argument (list 'epg-import-status-p import-status)))
  (aref (cdr import-status) 4))

(defun epg-import-status-sub-key (import-status)
  "Return t if the imported key contained new sub keys."
  (unless (eq (car-safe import-status) 'epg-import-status)
    (signal 'wrong-type-argument (list 'epg-import-status-p import-status)))
  (aref (cdr import-status) 5))

(defun epg-import-status-secret (import-status)
  "Return t if the imported key contained a secret key."
  (unless (eq (car-safe import-status) 'epg-import-status)
    (signal 'wrong-type-argument (list 'epg-import-status-p import-status)))
  (aref (cdr import-status) 6))

(defun epg-make-import-result (considered no-user-id imported imported-rsa
					  unchanged new-user-ids new-sub-keys
					  new-signatures new-revocations
					  secret-read secret-imported
					  secret-unchanged not-imported
					  imports)
  "Return an import result object."
  (cons 'epg-import-result (vector considered no-user-id imported imported-rsa
				   unchanged new-user-ids new-sub-keys
				   new-signatures new-revocations secret-read
				   secret-imported secret-unchanged
				   not-imported imports)))

(defun epg-import-result-considered (import-result)
  "Return the total number of considered keys."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 0))

(defun epg-import-result-no-user-id (import-result)
  "Return the number of keys without user ID."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 1))

(defun epg-import-result-imported (import-result)
  "Return the number of imported keys."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 2))

(defun epg-import-result-imported-rsa (import-result)
  "Return the number of imported RSA keys."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 3))

(defun epg-import-result-unchanged (import-result)
  "Return the number of unchanged keys."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 4))

(defun epg-import-result-new-user-ids (import-result)
  "Return the number of new user IDs."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 5))

(defun epg-import-result-new-sub-keys (import-result)
  "Return the number of new sub keys."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 6))

(defun epg-import-result-new-signatures (import-result)
  "Return the number of new signatures."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 7))

(defun epg-import-result-new-revocations (import-result)
  "Return the number of new revocations."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 8))

(defun epg-import-result-secret-read (import-result)
  "Return the total number of secret keys read."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 9))

(defun epg-import-result-secret-imported (import-result)
  "Return the number of imported secret keys."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 10))

(defun epg-import-result-secret-unchanged (import-result)
  "Return the number of unchanged secret keys."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 11))

(defun epg-import-result-not-imported (import-result)
  "Return the number of keys not imported."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 12))

(defun epg-import-result-imports (import-result)
  "Return the list of `epg-import-status' objects."
  (unless (eq (car-safe import-result) 'epg-import-result)
    (signal 'wrong-type-argument (list 'epg-import-result-p import-result)))
  (aref (cdr import-result) 13))

(defun epg-context-result-for (context name)
  "Return the result of CONTEXT associated with NAME."
  (cdr (assq name (epg-context-result context))))

(defun epg-context-set-result-for (context name value)
  "Set the result of CONTEXT associated with NAME to VALUE."
  (let* ((result (epg-context-result context))
	 (entry (assq name result)))
    (if entry
	(setcdr entry value)
      (epg-context-set-result context (cons (cons name value) result)))))

(defun epg-signature-to-string (signature)
  "Convert SIGNATURE to a human readable string."
  (let* ((user-id (cdr (assoc (epg-signature-key-id signature)
			      epg-user-id-alist)))
	 (pubkey-algorithm (epg-signature-pubkey-algorithm signature)))
    (concat
     (cond ((eq (epg-signature-status signature) 'good)
	    "Good signature from ")
	   ((eq (epg-signature-status signature) 'bad)
	    "Bad signature from ")
	   ((eq (epg-signature-status signature) 'expired)
	    "Expired signature from ")
	   ((eq (epg-signature-status signature) 'expired-key)
	    "Signature made by expired key ")
	   ((eq (epg-signature-status signature) 'revoked-key)
	    "Signature made by revoked key ")
	   ((eq (epg-signature-status signature) 'no-pubkey)
	    "No public key for "))
     (epg-signature-key-id signature)
     (if user-id
	 (concat " "
		 (if (stringp user-id)
		     user-id
		   (epg-decode-dn user-id)))
       "")
     (if (epg-signature-validity signature)
	 (format " (trust %s)"  (epg-signature-validity signature))
       "")
     (if (epg-signature-creation-time signature)
	 (format-time-string " created at %Y-%m-%dT%T%z"
			     (epg-signature-creation-time signature))
       "")
     (if pubkey-algorithm
	 (concat " using "
		 (or (cdr (assq pubkey-algorithm epg-pubkey-algorithm-alist))
		     (format "(unknown algorithm %d)" pubkey-algorithm)))
       ""))))

(defun epg-verify-result-to-string (verify-result)
  "Convert VERIFY-RESULT to a human readable string."
  (mapconcat #'epg-signature-to-string verify-result "\n"))

(defun epg-new-signature-to-string (new-signature)
  "Convert NEW-SIGNATURE to a human readable string."
  (concat
   (cond ((eq (epg-new-signature-type new-signature) 'detached)
	  "Detached signature ")
	 ((eq (epg-new-signature-type new-signature) 'clear)
	  "Cleartext signature ")
	 (t
	  "Signature "))
   (cdr (assq (epg-new-signature-pubkey-algorithm new-signature)
	      epg-pubkey-algorithm-alist))
   "/"
   (cdr (assq (epg-new-signature-digest-algorithm new-signature)
	      epg-digest-algorithm-alist))
   " "
   (format "%02X " (epg-new-signature-class new-signature))
   (epg-new-signature-fingerprint new-signature)))

(defun epg-import-result-to-string (import-result)
  "Convert IMPORT-RESULT to a human readable string."
  (concat (format "Total number processed: %d\n"
		  (epg-import-result-considered import-result))
	  (if (> (epg-import-result-not-imported import-result) 0)
	      (format "      skipped new keys: %d\n"
		      (epg-import-result-not-imported import-result)))
	  (if (> (epg-import-result-no-user-id import-result) 0)
	      (format "          w/o user IDs: %d\n"
		      (epg-import-result-no-user-id import-result)))
	  (if (> (epg-import-result-imported import-result) 0)
	      (concat (format "              imported: %d"
			      (epg-import-result-imported import-result))
		      (if (> (epg-import-result-imported-rsa import-result) 0)
			  (format "  (RSA: %d)"
				  (epg-import-result-imported-rsa
				   import-result)))
		      "\n"))
	  (if (> (epg-import-result-unchanged import-result) 0)
	      (format "             unchanged: %d\n"
		      (epg-import-result-unchanged import-result)))
	  (if (> (epg-import-result-new-user-ids import-result) 0)
	      (format "          new user IDs: %d\n"
		      (epg-import-result-new-user-ids import-result)))
	  (if (> (epg-import-result-new-sub-keys import-result) 0)
	      (format "           new subkeys: %d\n"
		      (epg-import-result-new-sub-keys import-result)))
	  (if (> (epg-import-result-new-signatures import-result) 0)
	      (format "        new signatures: %d\n"
		      (epg-import-result-new-signatures import-result)))
	  (if (> (epg-import-result-new-revocations import-result) 0)
	      (format "   new key revocations: %d\n"
		      (epg-import-result-new-revocations import-result)))
	  (if (> (epg-import-result-secret-read import-result) 0)
	      (format "      secret keys read: %d\n"
		      (epg-import-result-secret-read import-result)))
	  (if (> (epg-import-result-secret-imported import-result) 0)
	      (format "  secret keys imported: %d\n"
		      (epg-import-result-secret-imported import-result)))
	  (if (> (epg-import-result-secret-unchanged import-result) 0)
	      (format " secret keys unchanged: %d\n"
		      (epg-import-result-secret-unchanged import-result)))))

(defun epg-error-to-string (error)
  (cond
   ((eq (car error) 'exit)
    "Exit")
   ((eq (car error) 'quit)
    "Cancelled")
   ((eq (car error) 'no-data)
    (let ((entry (assq (cdr error) epg-no-data-reason-alist)))
      (if entry
	  (format "No data (%s)" (downcase (cdr entry)))
	"No data")))
   ((eq (car error) 'unexpected)
    (let ((entry (assq (cdr error) epg-unexpected-reason-alist)))
      (if entry
	  (format "Unexpected (%s)" (downcase (cdr entry)))
	"Unexpected")))
   ((eq (car error) 'bad-armor)
    "Bad armor")
   ((memq (car error) '(invalid-recipient invalid-signer))
    (concat
     (if (eq (car error) 'invalid-recipient)
	 "Unusable public key"
       "Unusable secret key")
     (let ((entry (assq 'requested (cdr error))))
       (if entry
	   (format ": %s" (cdr entry))
	 ": <unknown>"))
     (let ((entry (assq 'reason (cdr error))))
       (if (and entry
		(> (cdr entry) 0)	;no specific reason given
		(setq entry (assq (cdr entry)
				  epg-invalid-recipients-reason-alist)))
	   (format " (%s)" (downcase (cdr entry)))
	 ""))))
   ((eq (car error) 'no-pubkey)
    (format "No public key: %s" (cdr error)))
   ((eq (car error) 'no-seckey)
    (format "No secret key: %s" (cdr error)))
   ((eq (car error) 'no-recipients)
    "No recipients")
   ((eq (car error) 'no-signers)
    "No signers")
   ((eq (car error) 'delete-problem)
    (let ((entry (assq (cdr error) epg-delete-problem-reason-alist)))
      (if entry
	  (format "Delete problem (%s)" (downcase (cdr entry)))
	"Delete problem")))
   ((eq (car error) 'key-not-created)
    "Key not created")))

(defun epg-errors-to-string (errors)
  (mapconcat #'epg-error-to-string errors "; "))

(defun epg--start (context args)
  "Start `epg-gpg-program' in a subprocess with given ARGS."
  (if (and (epg-context-process context)
	   (eq (process-status (epg-context-process context)) 'run))
      (error "%s is already running in this context"
	     (if (eq (epg-context-protocol context) 'CMS)
		 epg-gpgsm-program
	       epg-gpg-program)))
  (let* ((args (append (list "--no-tty"
			     "--status-fd" "1"
			     "--yes")
		       (if (and (not (eq (epg-context-protocol context) 'CMS))
				(string-match ":" (or (getenv "GPG_AGENT_INFO")
						      "")))
			   '("--use-agent"))
		       (if (and (not (eq (epg-context-protocol context) 'CMS))
				(epg-context-progress-callback context))
			   '("--enable-progress-filter"))
		       (if epg-gpg-home-directory
			   (list "--homedir" epg-gpg-home-directory))
		       (unless (eq (epg-context-protocol context) 'CMS)
			 '("--command-fd" "0"))
		       (if (epg-context-armor context) '("--armor"))
		       (if (epg-context-textmode context) '("--textmode"))
		       (if (epg-context-output-file context)
			   (list "--output" (epg-context-output-file context)))
		       args))
	 (coding-system-for-write 'binary)
	 (coding-system-for-read 'binary)
	 process-connection-type
	 (orig-mode (default-file-modes))
	 (buffer (generate-new-buffer " *epg*"))
	 process)
    (if epg-debug
	(save-excursion
	  (unless epg-debug-buffer
	    (setq epg-debug-buffer (generate-new-buffer " *epg-debug*")))
	  (set-buffer epg-debug-buffer)
	  (goto-char (point-max))
	  (insert (format "%s %s\n"
			  (if (eq (epg-context-protocol context) 'CMS)
			      epg-gpgsm-program
			   epg-gpg-program)
			  (mapconcat #'identity args " ")))))
    (with-current-buffer buffer
      (if (fboundp 'set-buffer-multibyte)
	  (set-buffer-multibyte nil))
      (make-local-variable 'epg-last-status)
      (setq epg-last-status nil)
      (make-local-variable 'epg-read-point)
      (setq epg-read-point (point-min))
      (make-local-variable 'epg-process-filter-running)
      (setq epg-process-filter-running nil)
      (make-local-variable 'epg-pending-status-list)
      (setq epg-pending-status-list nil)
      (make-local-variable 'epg-key-id)
      (setq epg-key-id nil)
      (make-local-variable 'epg-context)
      (setq epg-context context))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  (setq process
		(apply #'start-process "epg" buffer
		       (if (eq (epg-context-protocol context) 'CMS)
			   epg-gpgsm-program
			 epg-gpg-program)
		       args)))
      (set-default-file-modes orig-mode))
    (set-process-filter process #'epg--process-filter)
    (epg-context-set-process context process)))

(defun epg--process-filter (process input)
  (if epg-debug
      (save-excursion
	(unless epg-debug-buffer
	  (setq epg-debug-buffer (generate-new-buffer " *epg-debug*")))
	(set-buffer epg-debug-buffer)
	(goto-char (point-max))
	(insert input)))
  (if (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
	(goto-char (point-max))
	(insert input)
	(unless epg-process-filter-running
	  (unwind-protect
	      (progn
		(setq epg-process-filter-running t)
		(goto-char epg-read-point)
		(beginning-of-line)
		(while (looking-at ".*\n") ;the input line finished
		  (if (looking-at "\\[GNUPG:] \\([A-Z_]+\\) ?\\(.*\\)")
		      (let* ((status (match-string 1))
			     (string (match-string 2))
			     (symbol (intern-soft (concat "epg--status-"
							  status))))
			(if (member status epg-pending-status-list)
			    (setq epg-pending-status-list nil))
			(if (and symbol
				 (fboundp symbol))
			    (funcall symbol epg-context string))
			(setq epg-last-status (cons status string))))
		  (forward-line)
		  (setq epg-read-point (point))))
	    (setq epg-process-filter-running nil))))))

(defun epg-read-output (context)
  "Read the output file CONTEXT and return the content as a string."
  (with-temp-buffer
    (if (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
    (if (file-exists-p (epg-context-output-file context))
	(let ((coding-system-for-read 'binary))
	  (insert-file-contents (epg-context-output-file context))
	  (buffer-string)))))

(defun epg-wait-for-status (context status-list)
  "Wait until one of elements in STATUS-LIST arrives."
  (with-current-buffer (process-buffer (epg-context-process context))
    (setq epg-pending-status-list status-list)
    (while (and (eq (process-status (epg-context-process context)) 'run)
		epg-pending-status-list)
      (accept-process-output (epg-context-process context) 1))
    (if epg-pending-status-list
	(epg-context-set-result-for
	 context 'error
	 (cons '(exit)
	       (epg-context-result-for context 'error))))))

(defun epg-wait-for-completion (context)
  "Wait until the `epg-gpg-program' process completes."
  (while (eq (process-status (epg-context-process context)) 'run)
    (accept-process-output (epg-context-process context) 1))
  ;; This line is needed to run the process-filter right now.
  (sleep-for 0.1)
  (epg-context-set-result-for
   context 'error
   (nreverse (epg-context-result-for context 'error))))

(defun epg-reset (context)
  "Reset the CONTEXT."
  (if (and (epg-context-process context)
	   (buffer-live-p (process-buffer (epg-context-process context))))
      (kill-buffer (process-buffer (epg-context-process context))))
  (epg-context-set-process context nil))

(defun epg-delete-output-file (context)
  "Delete the output file of CONTEXT."
  (if (and (epg-context-output-file context)
	   (file-exists-p (epg-context-output-file context)))
      (delete-file (epg-context-output-file context))))

(eval-and-compile
  (if (fboundp 'decode-coding-string)
      (defalias 'epg--decode-coding-string 'decode-coding-string)
    (defalias 'epg--decode-coding-string 'identity)))

(defun epg--status-USERID_HINT (_context string)
  (if (string-match "\\`\\([^ ]+\\) \\(.*\\)" string)
      (let* ((key-id (match-string 1 string))
	     (user-id (match-string 2 string))
	     (entry (assoc key-id epg-user-id-alist)))
	(condition-case nil
	    (setq user-id (epg--decode-coding-string
			   (epg--decode-percent-escape user-id)
			   'utf-8))
	  (error))
	(if entry
	    (setcdr entry user-id)
	  (setq epg-user-id-alist (cons (cons key-id user-id)
					epg-user-id-alist))))))

(defun epg--status-NEED_PASSPHRASE (_context string)
  (if (string-match "\\`\\([^ ]+\\)" string)
      (setq epg-key-id (match-string 1 string))))

(defun epg--status-NEED_PASSPHRASE_SYM (_context _string)
  (setq epg-key-id 'SYM))

(defun epg--status-NEED_PASSPHRASE_PIN (_context _string)
  (setq epg-key-id 'PIN))

(eval-and-compile
  (if (fboundp 'clear-string)
      (defalias 'epg--clear-string 'clear-string)
    (defun epg--clear-string (string)
      (fillarray string 0))))

(eval-and-compile
  (if (fboundp 'encode-coding-string)
      (defalias 'epg--encode-coding-string 'encode-coding-string)
    (defalias 'epg--encode-coding-string 'identity)))

(defun epg--status-GET_HIDDEN (context string)
  (when (and epg-key-id
	     (string-match "\\`passphrase\\." string))
    (unless (epg-context-passphrase-callback context)
      (error "passphrase-callback not set"))
    (let (inhibit-quit
	  passphrase
	  passphrase-with-new-line
	  encoded-passphrase-with-new-line)
      (unwind-protect
	  (condition-case nil
	      (progn
		(setq passphrase
		      (funcall
		       (car (epg-context-passphrase-callback context))
		       context
		       epg-key-id
		       (cdr (epg-context-passphrase-callback context))))
		(when passphrase
		  (setq passphrase-with-new-line (concat passphrase "\n"))
		  (epg--clear-string passphrase)
		  (setq passphrase nil)
		  (if epg-passphrase-coding-system
		      (progn
			(setq encoded-passphrase-with-new-line
			      (epg--encode-coding-string
			       passphrase-with-new-line
			       (coding-system-change-eol-conversion
				epg-passphrase-coding-system 'unix)))
			(epg--clear-string passphrase-with-new-line)
			(setq passphrase-with-new-line nil))
		    (setq encoded-passphrase-with-new-line
			  passphrase-with-new-line
			  passphrase-with-new-line nil))
		  (process-send-string (epg-context-process context)
				       encoded-passphrase-with-new-line)))
	    (quit
	     (epg-context-set-result-for
	      context 'error
	      (cons '(quit)
		    (epg-context-result-for context 'error)))
	     (delete-process (epg-context-process context))))
	(if passphrase
	    (epg--clear-string passphrase))
	(if passphrase-with-new-line
	    (epg--clear-string passphrase-with-new-line))
	(if encoded-passphrase-with-new-line
	    (epg--clear-string encoded-passphrase-with-new-line))))))

(defun epg--prompt-GET_BOOL (_context string)
  (let ((entry (assoc string epg-prompt-alist)))
    (y-or-n-p (if entry (cdr entry) (concat string "? ")))))

(defun epg--prompt-GET_BOOL-untrusted_key.override (_context _string)
  (y-or-n-p (if (and (equal (car epg-last-status) "USERID_HINT")
		     (string-match "\\`\\([^ ]+\\) \\(.*\\)"
				   (cdr epg-last-status)))
		(let* ((key-id (match-string 1 (cdr epg-last-status)))
		       (user-id (match-string 2 (cdr epg-last-status)))
		       (entry (assoc key-id epg-user-id-alist)))
		  (if entry
		      (setq user-id (cdr entry)))
		  (format "Untrusted key %s %s.  Use anyway? " key-id user-id))
	      "Use untrusted key anyway? ")))

(defun epg--status-GET_BOOL (context string)
  (let (inhibit-quit)
    (condition-case nil
	(if (funcall (or (intern-soft (concat "epg--prompt-GET_BOOL-" string))
			 #'epg--prompt-GET_BOOL)
		     context string)
	    (process-send-string (epg-context-process context) "y\n")
	  (process-send-string (epg-context-process context) "n\n"))
      (quit
       (epg-context-set-result-for
	context 'error
	(cons '(quit)
	      (epg-context-result-for context 'error)))
       (delete-process (epg-context-process context))))))

(defun epg--status-GET_LINE (context string)
  (let ((entry (assoc string epg-prompt-alist))
	inhibit-quit)
    (condition-case nil
	(process-send-string (epg-context-process context)
			     (concat (read-string
				      (if entry
					  (cdr entry)
					(concat string ": ")))
				     "\n"))
      (quit
       (epg-context-set-result-for
	context 'error
	(cons '(quit)
	      (epg-context-result-for context 'error)))
       (delete-process (epg-context-process context))))))

(defun epg--status-*SIG (context status string)
  (if (string-match "\\`\\([^ ]+\\) \\(.*\\)" string)
      (let* ((key-id (match-string 1 string))
	     (user-id (match-string 2 string))
	     (entry (assoc key-id epg-user-id-alist)))
	(epg-context-set-result-for
	 context
	 'verify
	 (cons (epg-make-signature status key-id)
	       (epg-context-result-for context 'verify)))
	(condition-case nil
	    (if (eq (epg-context-protocol context) 'CMS)
		(setq user-id (epg-dn-from-string user-id))
	      (setq user-id (epg--decode-coding-string
			     (epg--decode-percent-escape user-id)
			     'utf-8)))
	  (error))
	(if entry
	    (setcdr entry user-id)
	  (setq epg-user-id-alist
		(cons (cons key-id user-id) epg-user-id-alist))))
    (epg-context-set-result-for
     context
     'verify
     (cons (epg-make-signature status)
	   (epg-context-result-for context 'verify)))))

(defun epg--status-GOODSIG (context string)
  (epg--status-*SIG context 'good string))

(defun epg--status-EXPSIG (context string)
  (epg--status-*SIG context 'expired string))

(defun epg--status-EXPKEYSIG (context string)
  (epg--status-*SIG context 'expired-key string))

(defun epg--status-REVKEYSIG (context string)
  (epg--status-*SIG context 'revoked-key string))

(defun epg--status-BADSIG (context string)
  (epg--status-*SIG context 'bad string))

(defun epg--status-NO_PUBKEY (context string)
  (if (eq (epg-context-operation context) 'verify)
      (let ((signature (car (epg-context-result-for context 'verify))))
	(if (and signature
		 (eq (epg-signature-status signature) 'error)
		 (equal (epg-signature-key-id signature) string))
	    (epg-signature-set-status signature 'no-pubkey)))
    (epg-context-set-result-for
     context 'error
     (cons (cons 'no-pubkey string)
	   (epg-context-result-for context 'error)))))

(defun epg--status-NO_SECKEY (context string)
  (epg-context-set-result-for
   context 'error
   (cons (cons 'no-seckey string)
	 (epg-context-result-for context 'error))))

(defun epg--time-from-seconds (seconds)
  (let ((number-seconds (string-to-number (concat seconds ".0"))))
    (cons (floor (/ number-seconds 65536))
	  (floor (mod number-seconds 65536)))))

(defun epg--status-ERRSIG (context string)
  (if (string-match "\\`\\([^ ]+\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([0-9A-Fa-f][0-9A-Fa-f]\\) \\([^ ]+\\) \\([0-9]+\\)"
		    string)
      (let ((signature (epg-make-signature 'error)))
	(epg-context-set-result-for
	 context
	 'verify
	 (cons signature
	       (epg-context-result-for context 'verify)))
	(epg-signature-set-key-id
	 signature
	 (match-string 1 string))
	(epg-signature-set-pubkey-algorithm
	 signature
	 (string-to-number (match-string 2 string)))
	(epg-signature-set-digest-algorithm
	 signature
	 (string-to-number (match-string 3 string)))
	(epg-signature-set-class
	 signature
	 (string-to-number (match-string 4 string) 16))
	(epg-signature-set-creation-time
	 signature
	 (epg--time-from-seconds (match-string 5 string))))))

(defun epg--status-VALIDSIG (context string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (when (and signature
	       (eq (epg-signature-status signature) 'good)
	       (string-match "\\`\\([^ ]+\\) [^ ]+ \\([^ ]+\\) \\([^ ]+\\) \
\\([0-9]+\\) [^ ]+ \\([0-9]+\\) \\([0-9]+\\) \\([0-9A-Fa-f][0-9A-Fa-f]\\) \
\\(.*\\)"
			   string))
      (epg-signature-set-fingerprint
       signature
       (match-string 1 string))
      (epg-signature-set-creation-time
       signature
       (epg--time-from-seconds (match-string 2 string)))
      (unless (equal (match-string 3 string) "0")
	(epg-signature-set-expiration-time
	 signature
	 (epg--time-from-seconds (match-string 3 string))))
      (epg-signature-set-version
       signature
       (string-to-number (match-string 4 string)))
      (epg-signature-set-pubkey-algorithm
       signature
       (string-to-number (match-string 5 string)))
      (epg-signature-set-digest-algorithm
       signature
       (string-to-number (match-string 6 string)))
      (epg-signature-set-class
       signature
       (string-to-number (match-string 7 string) 16)))))

(defun epg--status-TRUST_UNDEFINED (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(epg-signature-set-validity signature 'undefined))))

(defun epg--status-TRUST_NEVER (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(epg-signature-set-validity signature 'never))))

(defun epg--status-TRUST_MARGINAL (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'marginal))
	(epg-signature-set-validity signature 'marginal))))

(defun epg--status-TRUST_FULLY (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(epg-signature-set-validity signature 'full))))

(defun epg--status-TRUST_ULTIMATE (context _string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if (and signature
	     (eq (epg-signature-status signature) 'good))
	(epg-signature-set-validity signature 'ultimate))))

(defun epg--status-NOTATION_NAME (context string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if signature
	(epg-signature-set-notations
	 signature
	 (cons (epg-make-sig-notation string nil t nil)
	       (epg-sig-notations signature))))))

(defun epg--status-NOTATION_DATA (context string)
  (let ((signature (car (epg-context-result-for context 'verify)))
	notation)
    (if (and signature
	     (setq notation (car (epg-sig-notations signature))))
	(epg-sig-notation-set-value notation string))))

(defun epg--status-POLICY_URL (context string)
  (let ((signature (car (epg-context-result-for context 'verify))))
    (if signature
	(epg-signature-set-notations
	 signature
	 (cons (epg-make-sig-notation nil string t nil)
	       (epg-sig-notations signature))))))

(defun epg--status-PROGRESS (context string)
  (if (and (epg-context-progress-callback context)
	   (string-match "\\`\\([^ ]+\\) \\([^ ]\\) \\([0-9]+\\) \\([0-9]+\\)"
			 string))
      (funcall (car (epg-context-progress-callback context))
	       context
	       (match-string 1 string)
	       (match-string 2 string)
	       (string-to-number (match-string 3 string))
	       (string-to-number (match-string 4 string))
	       (cdr (epg-context-progress-callback context)))))

(defun epg--status-ENC_TO (context string)
  (if (string-match "\\`\\([0-9A-Za-z]+\\) \\([0-9]+\\) \\([0-9]+\\)" string)
      (epg-context-set-result-for
       context 'encrypted-to
       (cons (list (match-string 1 string)
		   (string-to-number (match-string 2 string))
		   (string-to-number (match-string 3 string)))
	     (epg-context-result-for context 'encrypted-to)))))

(defun epg--status-DECRYPTION_FAILED (context _string)
  (epg-context-set-result-for context 'decryption-failed t))

(defun epg--status-DECRYPTION_OKAY (context _string)
  (epg-context-set-result-for context 'decryption-okay t))

(defun epg--status-NODATA (context string)
  (epg-context-set-result-for
   context 'error
   (cons (cons 'no-data (string-to-number string))
	 (epg-context-result-for context 'error))))

(defun epg--status-UNEXPECTED (context string)
  (epg-context-set-result-for
   context 'error
   (cons (cons 'unexpected (string-to-number string))
	 (epg-context-result-for context 'error))))

(defun epg--status-KEYEXPIRED (context string)
  (epg-context-set-result-for
   context 'key
   (cons (list 'key-expired (cons 'expiration-time
				  (epg--time-from-seconds string)))
	 (epg-context-result-for context 'key))))

(defun epg--status-KEYREVOKED (context _string)
  (epg-context-set-result-for
   context 'key
   (cons '(key-revoked)
	 (epg-context-result-for context 'key))))

(defun epg--status-BADARMOR (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(bad-armor)
	 (epg-context-result-for context 'error))))

(defun epg--status-INV_RECP (context string)
  (if (string-match "\\`\\([0-9]+\\) \\(.*\\)" string)
      (epg-context-set-result-for
       context 'error
       (cons (list 'invalid-recipient
		   (cons 'reason
			 (string-to-number (match-string 1 string)))
		   (cons 'requested
			 (match-string 2 string)))
	     (epg-context-result-for context 'error)))))

(defun epg--status-INV_SGNR (context string)
  (if (string-match "\\`\\([0-9]+\\) \\(.*\\)" string)
      (epg-context-set-result-for
       context 'error
       (cons (list 'invalid-signer
		   (cons 'reason
			 (string-to-number (match-string 1 string)))
		   (cons 'requested
			 (match-string 2 string)))
	     (epg-context-result-for context 'error)))))

(defun epg--status-NO_RECP (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(no-recipients)
	 (epg-context-result-for context 'error))))

(defun epg--status-NO_SGNR (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(no-signers)
	 (epg-context-result-for context 'error))))

(defun epg--status-DELETE_PROBLEM (context string)
  (if (string-match "\\`\\([0-9]+\\)" string)
      (epg-context-set-result-for
       context 'error
       (cons (cons 'delete-problem
		   (string-to-number (match-string 1 string)))
	     (epg-context-result-for context 'error)))))

(defun epg--status-SIG_CREATED (context string)
  (if (string-match "\\`\\([DCS]\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([0-9A-Fa-F][0-9A-Fa-F]\\) \\(.*\\) " string)
      (epg-context-set-result-for
       context 'sign
       (cons (epg-make-new-signature
	      (cdr (assq (aref (match-string 1 string) 0)
			 epg-new-signature-type-alist))
	      (string-to-number (match-string 2 string))
	      (string-to-number (match-string 3 string))
	      (string-to-number (match-string 4 string) 16)
	      (epg--time-from-seconds (match-string 5 string))
	      (substring string (match-end 0)))
	     (epg-context-result-for context 'sign)))))

(defun epg--status-KEY_CREATED (context string)
  (if (string-match "\\`\\([BPS]\\) \\([^ ]+\\)" string)
      (epg-context-set-result-for
       context 'generate-key
       (cons (list (cons 'type (string-to-char (match-string 1 string)))
		   (cons 'fingerprint (match-string 2 string)))
	     (epg-context-result-for context 'generate-key)))))

(defun epg--status-KEY_NOT_CREATED (context _string)
  (epg-context-set-result-for
   context 'error
   (cons '(key-not-created)
	 (epg-context-result-for context 'error))))

(defun epg--status-IMPORTED (_context string)
  (if (string-match "\\`\\([^ ]+\\) \\(.*\\)" string)
      (let* ((key-id (match-string 1 string))
	     (user-id (match-string 2 string))
	     (entry (assoc key-id epg-user-id-alist)))
	(condition-case nil
	    (setq user-id (epg--decode-coding-string
			   (epg--decode-percent-escape user-id)
			   'utf-8))
	  (error))
	(if entry
	    (setcdr entry user-id)
	  (setq epg-user-id-alist (cons (cons key-id user-id)
					epg-user-id-alist))))))

(defun epg--status-IMPORT_OK (context string)
  (if (string-match "\\`\\([0-9]+\\)\\( \\(.+\\)\\)?" string)
      (let ((reason (string-to-number (match-string 1 string))))
	(epg-context-set-result-for
	 context 'import-status
	 (cons (epg-make-import-status (if (match-beginning 2)
					   (match-string 3 string))
				       nil
				       (/= (logand reason 1) 0)
				       (/= (logand reason 2) 0)
				       (/= (logand reason 4) 0)
				       (/= (logand reason 8) 0)
				       (/= (logand reason 16) 0))
	       (epg-context-result-for context 'import-status))))))

(defun epg--status-IMPORT_PROBLEM (context string)
  (if (string-match "\\`\\([0-9]+\\)\\( \\(.+\\)\\)?" string)
      (epg-context-set-result-for
       context 'import-status
       (cons (epg-make-import-status
	      (if (match-beginning 2)
		  (match-string 3 string))
	      (string-to-number (match-string 1 string)))
	     (epg-context-result-for context 'import-status)))))

(defun epg--status-IMPORT_RES (context string)
  (when (string-match "\\`\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \
\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" string)
    (epg-context-set-result-for
     context 'import
     (epg-make-import-result (string-to-number (match-string 1 string))
			     (string-to-number (match-string 2 string))
			     (string-to-number (match-string 3 string))
			     (string-to-number (match-string 4 string))
			     (string-to-number (match-string 5 string))
			     (string-to-number (match-string 6 string))
			     (string-to-number (match-string 7 string))
			     (string-to-number (match-string 8 string))
			     (string-to-number (match-string 9 string))
			     (string-to-number (match-string 10 string))
			     (string-to-number (match-string 11 string))
			     (string-to-number (match-string 12 string))
			     (string-to-number (match-string 13 string))
			     (epg-context-result-for context 'import-status)))
    (epg-context-set-result-for context 'import-status nil)))

(defun epg-passphrase-callback-function (context key-id _handback)
  (if (eq key-id 'SYM)
      (read-passwd "Passphrase for symmetric encryption: "
		   (eq (epg-context-operation context) 'encrypt))
    (read-passwd
     (if (eq key-id 'PIN)
	"Passphrase for PIN: "
       (let ((entry (assoc key-id epg-user-id-alist)))
	 (if entry
	     (format "Passphrase for %s %s: " key-id (cdr entry))
	   (format "Passphrase for %s: " key-id)))))))

(make-obsolete 'epg-passphrase-callback-function
	       'epa-passphrase-callback-function "23.1")

(defun epg--list-keys-1 (context name mode)
  (let ((args (append (if epg-gpg-home-directory
			  (list "--homedir" epg-gpg-home-directory))
		      '("--with-colons" "--no-greeting" "--batch"
			"--with-fingerprint" "--with-fingerprint")
		      (unless (eq (epg-context-protocol context) 'CMS)
			'("--fixed-list-mode"))))
	(list-keys-option (if (memq mode '(t secret))
			      "--list-secret-keys"
			    (if (memq mode '(nil public))
				"--list-keys"
			      "--list-sigs")))
	(coding-system-for-read 'binary)
	keys string field index)
    (if name
	(progn
	  (unless (listp name)
	    (setq name (list name)))
	  (while name
	    (setq args (append args (list list-keys-option (car name)))
		  name (cdr name))))
      (setq args (append args (list list-keys-option))))
    (with-temp-buffer
      (apply #'call-process
	     (if (eq (epg-context-protocol context) 'CMS)
		 epg-gpgsm-program
	       epg-gpg-program)
	     nil (list t nil) nil args)
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
	(setq keys (cons (make-vector 15 nil) keys)
	      string (match-string 0)
	      index 0
	      field 0)
	(while (eq index
		   (string-match "\\([^:]+\\)?:" string index))
	  (setq index (match-end 0))
	  (aset (car keys) field (match-string 1 string))
	  (setq field (1+ field))))
      (nreverse keys))))

(defun epg--make-sub-key-1 (line)
  (epg-make-sub-key
   (if (aref line 1)
       (cdr (assq (string-to-char (aref line 1)) epg-key-validity-alist)))
   (delq nil
	 (mapcar (lambda (char) (cdr (assq char epg-key-capablity-alist)))
		 (aref line 11)))
   (member (aref line 0) '("sec" "ssb"))
   (string-to-number (aref line 3))
   (string-to-number (aref line 2))
   (aref line 4)
   (epg--time-from-seconds (aref line 5))
   (if (aref line 6)
       (epg--time-from-seconds (aref line 6)))))

(defun epg-list-keys (context &optional name mode)
  "Return a list of epg-key objects matched with NAME.
If MODE is nil or 'public, only public keyring should be searched.
If MODE is t or 'secret, only secret keyring should be searched.
Otherwise, only public keyring should be searched and the key
signatures should be included.
NAME is either a string or a list of strings."
  (let ((lines (epg--list-keys-1 context name mode))
	keys cert pointer pointer-1 index string)
    (while lines
      (cond
       ((member (aref (car lines) 0) '("pub" "sec" "crt" "crs"))
	(setq cert (member (aref (car lines) 0) '("crt" "crs"))
	      keys (cons (epg-make-key
			  (if (aref (car lines) 8)
			      (cdr (assq (string-to-char (aref (car lines) 8))
					 epg-key-validity-alist))))
			 keys))
	(epg-key-set-sub-key-list
	 (car keys)
	 (cons (epg--make-sub-key-1 (car lines))
	       (epg-key-sub-key-list (car keys)))))
       ((member (aref (car lines) 0) '("sub" "ssb"))
	(epg-key-set-sub-key-list
	 (car keys)
	 (cons (epg--make-sub-key-1 (car lines))
	       (epg-key-sub-key-list (car keys)))))
       ((equal (aref (car lines) 0) "uid")
	;; Decode the UID name as a backslash escaped UTF-8 string,
	;; generated by GnuPG/GpgSM.
	(setq string (copy-sequence (aref (car lines) 9))
	      index 0)
	(while (string-match "\"" string index)
	  (setq string (replace-match "\\\"" t t string)
		index (1+ (match-end 0))))
	(condition-case nil
	    (setq string (epg--decode-coding-string
			  (car (read-from-string (concat "\"" string "\"")))
			  'utf-8))
	  (error
	   (setq string (aref (car lines) 9))))
	(epg-key-set-user-id-list
	 (car keys)
	 (cons (epg-make-user-id
		(if (aref (car lines) 1)
		    (cdr (assq (string-to-char (aref (car lines) 1))
			       epg-key-validity-alist)))
		(if cert
		    (condition-case nil
			(epg-dn-from-string string)
		      (error string))
		  string))
	       (epg-key-user-id-list (car keys)))))
       ((equal (aref (car lines) 0) "fpr")
	(epg-sub-key-set-fingerprint (car (epg-key-sub-key-list (car keys)))
				     (aref (car lines) 9)))
       ((equal (aref (car lines) 0) "sig")
	(epg-user-id-set-signature-list
	 (car (epg-key-user-id-list (car keys)))
	 (cons
	  (epg-make-key-signature
	   (if (aref (car lines) 1)
	       (cdr (assq (string-to-char (aref (car lines) 1))
			  epg-key-validity-alist)))
	   (string-to-number (aref (car lines) 3))
	   (aref (car lines) 4)
	   (epg--time-from-seconds (aref (car lines) 5))
	   (epg--time-from-seconds (aref (car lines) 6))
	   (aref (car lines) 9)
	   (string-to-number (aref (car lines) 10) 16)
	   (eq (aref (aref (car lines) 10) 2) ?x))
	  (epg-user-id-signature-list
	   (car (epg-key-user-id-list (car keys))))))))
      (setq lines (cdr lines)))
    (setq keys (nreverse keys)
	  pointer keys)
    (while pointer
      (epg-key-set-sub-key-list
       (car pointer)
       (nreverse (epg-key-sub-key-list (car pointer))))
      (setq pointer-1 (epg-key-set-user-id-list
			  (car pointer)
			  (nreverse (epg-key-user-id-list (car pointer)))))
      (while pointer-1
	(epg-user-id-set-signature-list
	 (car pointer-1)
	 (nreverse (epg-user-id-signature-list (car pointer-1))))
	(setq pointer-1 (cdr pointer-1)))
      (setq pointer (cdr pointer)))
    keys))

(eval-and-compile
  (if (fboundp 'make-temp-file)
      (defalias 'epg--make-temp-file 'make-temp-file)
    (defvar temporary-file-directory)
    ;; stolen from poe.el.
    (defun epg--make-temp-file (prefix)
      "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file."
      (let ((orig-modes (default-file-modes))
	    tempdir tempfile)
	(setq prefix (expand-file-name prefix
				       (if (featurep 'xemacs)
					   (temp-directory)
					 temporary-file-directory)))
	(unwind-protect
	    (let (file)
	      ;; First, create a temporary directory.
	      (set-default-file-modes #o700)
	      (while (condition-case ()
			 (progn
			   (setq tempdir (make-temp-name
					  (concat
					   (file-name-directory prefix)
					   "DIR")))
			   ;; return nil or signal an error.
			   (make-directory tempdir))
		       ;; let's try again.
		       (file-already-exists t)))
	      ;; Second, create a temporary file in the tempdir.
	      ;; There *is* a race condition between `make-temp-name'
	      ;; and `write-region', but we don't care it since we are
	      ;; in a private directory now.
	      (setq tempfile (make-temp-name (concat tempdir "/EMU")))
	      (write-region "" nil tempfile nil 'silent)
	      ;; Finally, make a hard-link from the tempfile.
	      (while (condition-case ()
			 (progn
			   (setq file (make-temp-name prefix))
			   ;; return nil or signal an error.
			   (add-name-to-file tempfile file))
		       ;; let's try again.
		       (file-already-exists t)))
	      file)
	  (set-default-file-modes orig-modes)
	  ;; Cleanup the tempfile.
	  (and tempfile
	       (file-exists-p tempfile)
	       (delete-file tempfile))
	  ;; Cleanup the tempdir.
	  (and tempdir
	       (file-directory-p tempdir)
	       (delete-directory tempdir)))))))

(defun epg--args-from-sig-notations (notations)
  (apply #'nconc
	 (mapcar
	  (lambda (notation)
	    (if (and (epg-sig-notation-name notation)
		     (not (epg-sig-notation-human-readable notation)))
		(error "Unreadable"))
	    (if (epg-sig-notation-name notation)
		(list "--sig-notation"
		      (if (epg-sig-notation-critical notation)
			  (concat "!" (epg-sig-notation-name notation)
				  "=" (epg-sig-notation-value notation))
			(concat (epg-sig-notation-name notation)
				"=" (epg-sig-notation-value notation))))
	      (list "--sig-policy-url"
		    (if (epg-sig-notation-critical notation)
			(concat "!" (epg-sig-notation-value notation))
		      (epg-sig-notation-value notation)))))
	  notations)))

(defun epg-cancel (context)
  (if (buffer-live-p (process-buffer (epg-context-process context)))
      (with-current-buffer (process-buffer (epg-context-process context))
	(epg-context-set-result-for
	 epg-context 'error
	 (cons '(quit)
	       (epg-context-result-for epg-context 'error)))))
  (if (eq (process-status (epg-context-process context)) 'run)
      (delete-process (epg-context-process context))))

(defun epg-start-decrypt (context cipher)
  "Initiate a decrypt operation on CIPHER.
CIPHER must be a file data object.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-decrypt-file' or `epg-decrypt-string' instead."
  (unless (epg-data-file cipher)
    (error "Not a file"))
  (epg-context-set-operation context 'decrypt)
  (epg-context-set-result context nil)
  (epg--start context (list "--decrypt" "--" (epg-data-file cipher)))
  ;; `gpgsm' does not read passphrase from stdin, so waiting is not needed.
  (unless (eq (epg-context-protocol context) 'CMS)
    (epg-wait-for-status context '("BEGIN_DECRYPTION"))))

(defun epg--check-error-for-decrypt (context)
  (let ((errors (epg-context-result-for context 'error)))
    (if (epg-context-result-for context 'decryption-failed)
	(signal 'epg-error
		(list "Decryption failed" (epg-errors-to-string errors))))
    (unless (epg-context-result-for context 'decryption-okay)
      (signal 'epg-error
	      (list "Can't decrypt" (epg-errors-to-string errors))))))

(defun epg-decrypt-file (context cipher plain)
  "Decrypt a file CIPHER and store the result to a file PLAIN.
If PLAIN is nil, it returns the result as a string."
  (unwind-protect
      (progn
	(if plain
	    (epg-context-set-output-file context plain)
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output")))
	(epg-start-decrypt context (epg-make-data-from-file cipher))
	(epg-wait-for-completion context)
	(epg--check-error-for-decrypt context)
	(unless plain
	  (epg-read-output context)))
    (unless plain
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-decrypt-string (context cipher)
  "Decrypt a string CIPHER and return the plain text."
  (let ((input-file (epg--make-temp-file "epg-input"))
	(coding-system-for-write 'binary))
    (unwind-protect
	(progn
	  (write-region cipher nil input-file nil 'quiet)
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output"))
	  (epg-start-decrypt context (epg-make-data-from-file input-file))
	  (epg-wait-for-completion context)
	  (epg--check-error-for-decrypt context)
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if (file-exists-p input-file)
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-verify (context signature &optional signed-text)
  "Initiate a verify operation on SIGNATURE.
SIGNATURE and SIGNED-TEXT are a data object if they are specified.

For a detached signature, both SIGNATURE and SIGNED-TEXT should be set.
For a normal or a cleartext signature, SIGNED-TEXT should be nil.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-verify-file' or `epg-verify-string' instead."
  (epg-context-set-operation context 'verify)
  (epg-context-set-result context nil)
  (if signed-text
      ;; Detached signature.
      (if (epg-data-file signed-text)
	  (epg--start context (list "--verify" "--" (epg-data-file signature)
				   (epg-data-file signed-text)))
	(epg--start context (list "--verify" "--" (epg-data-file signature)
				  "-"))
	(if (eq (process-status (epg-context-process context)) 'run)
	    (process-send-string (epg-context-process context)
				 (epg-data-string signed-text)))
	(if (eq (process-status (epg-context-process context)) 'run)
	    (process-send-eof (epg-context-process context))))
    ;; Normal (or cleartext) signature.
    (if (epg-data-file signature)
	(epg--start context (if (eq (epg-context-protocol context) 'CMS)
				(list "--verify" "--" (epg-data-file signature))
			      (list "--" (epg-data-file signature))))
      (epg--start context (if (eq (epg-context-protocol context) 'CMS)
			      '("--verify" "-")
			    '("-")))
      (if (eq (process-status (epg-context-process context)) 'run)
	  (process-send-string (epg-context-process context)
			       (epg-data-string signature)))
      (if (eq (process-status (epg-context-process context)) 'run)
	  (process-send-eof (epg-context-process context))))))

(defun epg-verify-file (context signature &optional signed-text plain)
  "Verify a file SIGNATURE.
SIGNED-TEXT and PLAIN are also a file if they are specified.

For a detached signature, both SIGNATURE and SIGNED-TEXT should be
string.  For a normal or a cleartext signature, SIGNED-TEXT should be
nil.  In the latter case, if PLAIN is specified, the plaintext is
stored into the file after successful verification."
  (unwind-protect
      (progn
	(if plain
	    (epg-context-set-output-file context plain)
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output")))
	(if signed-text
	    (epg-start-verify context
			      (epg-make-data-from-file signature)
			      (epg-make-data-from-file signed-text))
	  (epg-start-verify context
			    (epg-make-data-from-file signature)))
	(epg-wait-for-completion context)
	(unless plain
	  (epg-read-output context)))
    (unless plain
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-verify-string (context signature &optional signed-text)
  "Verify a string SIGNATURE.
SIGNED-TEXT is a string if it is specified.

For a detached signature, both SIGNATURE and SIGNED-TEXT should be
string.  For a normal or a cleartext signature, SIGNED-TEXT should be
nil.  In the latter case, this function returns the plaintext after
successful verification."
  (let ((coding-system-for-write 'binary)
	input-file)
    (unwind-protect
	(progn
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output"))
	  (if signed-text
	      (progn
		(setq input-file (epg--make-temp-file "epg-signature"))
		(write-region signature nil input-file nil 'quiet)
		(epg-start-verify context
				  (epg-make-data-from-file input-file)
				  (epg-make-data-from-string signed-text)))
	    (epg-start-verify context (epg-make-data-from-string signature)))
	  (epg-wait-for-completion context)
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if (and input-file
	       (file-exists-p input-file))
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-sign (context plain &optional mode)
  "Initiate a sign operation on PLAIN.
PLAIN is a data object.

If optional 3rd argument MODE is t or 'detached, it makes a detached signature.
If it is nil or 'normal, it makes a normal signature.
Otherwise, it makes a cleartext signature.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-sign-file' or `epg-sign-string' instead."
  (epg-context-set-operation context 'sign)
  (epg-context-set-result context nil)
  (unless (memq mode '(t detached nil normal)) ;i.e. cleartext
    (epg-context-set-armor context nil)
    (epg-context-set-textmode context nil))
  (epg--start context
	     (append (list (if (memq mode '(t detached))
			       "--detach-sign"
			     (if (memq mode '(nil normal))
				 "--sign"
			       "--clearsign")))
		     (apply #'nconc
			    (mapcar
			     (lambda (signer)
			       (list "-u"
				     (epg-sub-key-id
				      (car (epg-key-sub-key-list signer)))))
			     (epg-context-signers context)))
		     (epg--args-from-sig-notations
		      (epg-context-sig-notations context))
		     (if (epg-data-file plain)
			 (list "--" (epg-data-file plain)))))
  ;; `gpgsm' does not read passphrase from stdin, so waiting is not needed.
  (unless (eq (epg-context-protocol context) 'CMS)
    (epg-wait-for-status context '("BEGIN_SIGNING")))
  (when (epg-data-string plain)
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string plain)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg-sign-file (context plain signature &optional mode)
  "Sign a file PLAIN and store the result to a file SIGNATURE.
If SIGNATURE is nil, it returns the result as a string.
If optional 3rd argument MODE is t or 'detached, it makes a detached signature.
If it is nil or 'normal, it makes a normal signature.
Otherwise, it makes a cleartext signature."
  (unwind-protect
      (progn
	(if signature
	    (epg-context-set-output-file context signature)
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output")))
	(epg-start-sign context (epg-make-data-from-file plain) mode)
	(epg-wait-for-completion context)
	(unless (epg-context-result-for context 'sign)
	  (let ((errors (epg-context-result-for context 'error)))
	    (signal 'epg-error
		    (list "Sign failed" (epg-errors-to-string errors)))))
	(unless signature
	  (epg-read-output context)))
    (unless signature
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-sign-string (context plain &optional mode)
  "Sign a string PLAIN and return the output as string.
If optional 3rd argument MODE is t or 'detached, it makes a detached signature.
If it is nil or 'normal, it makes a normal signature.
Otherwise, it makes a cleartext signature."
  (let ((input-file
	 (unless (or (eq (epg-context-protocol context) 'CMS)
		     (condition-case nil
			 (progn
			   (epg-check-configuration (epg-configuration))
			   t)
		       (error)))
	   (epg--make-temp-file "epg-input")))
	(coding-system-for-write 'binary))
    (unwind-protect
	(progn
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output"))
	  (if input-file
	      (write-region plain nil input-file nil 'quiet))
	  (epg-start-sign context
			  (if input-file
			      (epg-make-data-from-file input-file)
			    (epg-make-data-from-string plain))
			  mode)
	  (epg-wait-for-completion context)
	  (unless (epg-context-result-for context 'sign)
	    (if (epg-context-result-for context 'error)
		(let ((errors (epg-context-result-for context 'error)))
		  (signal 'epg-error
			  (list "Sign failed" (epg-errors-to-string errors))))))
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if input-file
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-encrypt (context plain recipients
				  &optional sign always-trust)
  "Initiate an encrypt operation on PLAIN.
PLAIN is a data object.
If RECIPIENTS is nil, it performs symmetric encryption.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-encrypt-file' or `epg-encrypt-string' instead."
  (epg-context-set-operation context 'encrypt)
  (epg-context-set-result context nil)
  (epg--start context
	     (append (if always-trust '("--always-trust"))
		     (if recipients '("--encrypt") '("--symmetric"))
		     (if sign '("--sign"))
		     (if sign
			 (apply #'nconc
				(mapcar
				 (lambda (signer)
				   (list "-u"
					 (epg-sub-key-id
					  (car (epg-key-sub-key-list
						signer)))))
				 (epg-context-signers context))))
		     (if sign
			 (epg--args-from-sig-notations
			  (epg-context-sig-notations context)))
		     (apply #'nconc
			    (mapcar
			     (lambda (recipient)
			       (list "-r"
				     (epg-sub-key-id
				      (car (epg-key-sub-key-list recipient)))))
			     recipients))
		     (if (epg-data-file plain)
			 (list "--" (epg-data-file plain)))))
  ;; `gpgsm' does not read passphrase from stdin, so waiting is not needed.
  (unless (eq (epg-context-protocol context) 'CMS)
    (if sign
	(epg-wait-for-status context '("BEGIN_SIGNING"))
      (epg-wait-for-status context '("BEGIN_ENCRYPTION"))))
  (when (epg-data-string plain)
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string plain)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg-encrypt-file (context plain recipients
				 cipher &optional sign always-trust)
  "Encrypt a file PLAIN and store the result to a file CIPHER.
If CIPHER is nil, it returns the result as a string.
If RECIPIENTS is nil, it performs symmetric encryption."
  (unwind-protect
      (progn
	(if cipher
	    (epg-context-set-output-file context cipher)
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output")))
	(epg-start-encrypt context (epg-make-data-from-file plain)
			   recipients sign always-trust)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if (and sign
		   (not (epg-context-result-for context 'sign)))
	      (signal 'epg-error
		      (list "Sign failed" (epg-errors-to-string errors))))
	  (if errors
	      (signal 'epg-error
		      (list "Encrypt failed" (epg-errors-to-string errors)))))
	(unless cipher
	  (epg-read-output context)))
    (unless cipher
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-encrypt-string (context plain recipients
				   &optional sign always-trust)
  "Encrypt a string PLAIN.
If RECIPIENTS is nil, it performs symmetric encryption."
  (let ((input-file
	 (unless (or (not sign)
		     (eq (epg-context-protocol context) 'CMS)
		     (condition-case nil
			 (progn
			   (epg-check-configuration (epg-configuration))
			   t)
		       (error)))
	   (epg--make-temp-file "epg-input")))
	(coding-system-for-write 'binary))
    (unwind-protect
	(progn
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output"))
	  (if input-file
	      (write-region plain nil input-file nil 'quiet))
	  (epg-start-encrypt context
			     (if input-file
				 (epg-make-data-from-file input-file)
			       (epg-make-data-from-string plain))
			     recipients sign always-trust)
	  (epg-wait-for-completion context)
	  (let ((errors (epg-context-result-for context 'error)))
	    (if (and sign
		     (not (epg-context-result-for context 'sign)))
		(signal 'epg-error
			(list "Sign failed" (epg-errors-to-string errors))))
	    (if errors
		(signal 'epg-error
			(list "Encrypt failed" (epg-errors-to-string errors)))))
	  (epg-read-output context))
      (epg-delete-output-file context)
      (if input-file
	  (delete-file input-file))
      (epg-reset context))))

(defun epg-start-export-keys (context keys)
  "Initiate an export keys operation.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-export-keys-to-file' or `epg-export-keys-to-string' instead."
  (epg-context-set-operation context 'export-keys)
  (epg-context-set-result context nil)
  (epg--start context (cons "--export"
			   (mapcar
			    (lambda (key)
			      (epg-sub-key-id
			       (car (epg-key-sub-key-list key))))
			    keys))))

(defun epg-export-keys-to-file (context keys file)
  "Extract public KEYS."
  (unwind-protect
      (progn
	(if file
	    (epg-context-set-output-file context file)
	  (epg-context-set-output-file context
				       (epg--make-temp-file "epg-output")))
	(epg-start-export-keys context keys)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Export keys failed"
			    (epg-errors-to-string errors)))))
	(unless file
	  (epg-read-output context)))
    (unless file
      (epg-delete-output-file context))
    (epg-reset context)))

(defun epg-export-keys-to-string (context keys)
  "Extract public KEYS and return them as a string."
  (epg-export-keys-to-file context keys nil))

(defun epg-start-import-keys (context keys)
  "Initiate an import keys operation.
KEYS is a data object.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-import-keys-from-file' or `epg-import-keys-from-string' instead."
  (epg-context-set-operation context 'import-keys)
  (epg-context-set-result context nil)
  (epg--start context (if (epg-data-file keys)
			  (list "--import" "--" (epg-data-file keys))
			(list "--import")))
  (when (epg-data-string keys)
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string keys)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg--import-keys-1 (context keys)
  (unwind-protect
      (progn
	(epg-start-import-keys context keys)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Import keys failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg-import-keys-from-file (context keys)
  "Add keys from a file KEYS."
  (epg--import-keys-1 context (epg-make-data-from-file keys)))

(defun epg-import-keys-from-string (context keys)
  "Add keys from a string KEYS."
  (epg--import-keys-1 context (epg-make-data-from-string keys)))

(defun epg-start-receive-keys (context key-id-list)
  "Initiate a receive key operation.
KEY-ID-LIST is a list of key IDs.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-receive-keys' instead."
  (epg-context-set-operation context 'receive-keys)
  (epg-context-set-result context nil)
  (epg--start context (cons "--recv-keys" key-id-list)))

(defun epg-receive-keys (context keys)
  "Add keys from server.
KEYS is a list of key IDs"
  (unwind-protect
      (progn
	(epg-start-receive-keys context keys)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Receive keys failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defalias 'epg-import-keys-from-server 'epg-receive-keys)

(defun epg-start-delete-keys (context keys &optional allow-secret)
  "Initiate a delete keys operation.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-delete-keys' instead."
  (epg-context-set-operation context 'delete-keys)
  (epg-context-set-result context nil)
  (epg--start context (cons (if allow-secret
			       "--delete-secret-key"
			     "--delete-key")
			    (mapcar
			     (lambda (key)
			       (epg-sub-key-id
				(car (epg-key-sub-key-list key))))
			     keys))))

(defun epg-delete-keys (context keys &optional allow-secret)
  "Delete KEYS from the key ring."
  (unwind-protect
      (progn
	(epg-start-delete-keys context keys allow-secret)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Delete keys failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg-start-sign-keys (context keys &optional local)
  "Initiate a sign keys operation.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-sign-keys' instead."
  (epg-context-set-operation context 'sign-keys)
  (epg-context-set-result context nil)
  (epg--start context (cons (if local
			       "--lsign-key"
			     "--sign-key")
			   (mapcar
			    (lambda (key)
			      (epg-sub-key-id
			       (car (epg-key-sub-key-list key))))
			    keys))))
(make-obsolete 'epg-start-sign-keys "do not use." "23.1")

(defun epg-sign-keys (context keys &optional local)
  "Sign KEYS from the key ring."
  (unwind-protect
      (progn
	(epg-start-sign-keys context keys local)
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Sign keys failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))
(make-obsolete 'epg-sign-keys "do not use." "23.1")

(defun epg-start-generate-key (context parameters)
  "Initiate a key generation.
PARAMETERS specifies parameters for the key.

If you use this function, you will need to wait for the completion of
`epg-gpg-program' by using `epg-wait-for-completion' and call
`epg-reset' to clear a temporary output file.
If you are unsure, use synchronous version of this function
`epg-generate-key-from-file' or `epg-generate-key-from-string' instead."
  (epg-context-set-operation context 'generate-key)
  (epg-context-set-result context nil)
  (if (epg-data-file parameters)
      (epg--start context (list "--batch" "--genkey" "--"
			       (epg-data-file parameters)))
    (epg--start context '("--batch" "--genkey"))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-string (epg-context-process context)
			     (epg-data-string parameters)))
    (if (eq (process-status (epg-context-process context)) 'run)
	(process-send-eof (epg-context-process context)))))

(defun epg-generate-key-from-file (context parameters)
  "Generate a new key pair.
PARAMETERS is a file which tells how to create the key."
  (unwind-protect
      (progn
	(epg-start-generate-key context (epg-make-data-from-file parameters))
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Generate key failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg-generate-key-from-string (context parameters)
  "Generate a new key pair.
PARAMETERS is a string which tells how to create the key."
  (unwind-protect
      (progn
	(epg-start-generate-key context (epg-make-data-from-string parameters))
	(epg-wait-for-completion context)
	(let ((errors (epg-context-result-for context 'error)))
	  (if errors
	      (signal 'epg-error
		      (list "Generate key failed"
			    (epg-errors-to-string errors))))))
    (epg-reset context)))

(defun epg--decode-percent-escape (string)
  (let ((index 0))
    (while (string-match "%\\(\\(%\\)\\|\\([0-9A-Fa-f][0-9A-Fa-f]\\)\\)"
			 string index)
      (if (match-beginning 2)
	  (setq string (replace-match "%" t t string)
		index (1- (match-end 0)))
	(setq string (replace-match
		      (string (string-to-number (match-string 3 string) 16))
		      t t string)
	      index (- (match-end 0) 2))))
    string))

(defun epg--decode-hexstring (string)
  (let ((index 0))
    (while (eq index (string-match "[0-9A-Fa-f][0-9A-Fa-f]" string index))
      (setq string (replace-match (string (string-to-number
					   (match-string 0 string) 16))
				  t t string)
	    index (1- (match-end 0))))
    string))

(defun epg--decode-quotedstring (string)
  (let ((index 0))
    (while (string-match "\\\\\\(\\([,=+<>#;\\\"]\\)\\|\
\\([0-9A-Fa-f][0-9A-Fa-f]\\)\\)"
			 string index)
      (if (match-beginning 2)
	  (setq string (replace-match "\\2" t nil string)
		index (1- (match-end 0)))
	(if (match-beginning 3)
	    (setq string (replace-match (string (string-to-number
						 (match-string 0 string) 16))
					t t string)
		  index (- (match-end 0) 2)))))
    string))

(defun epg-dn-from-string (string)
  "Parse STRING as LADPv3 Distinguished Names (RFC2253).
The return value is an alist mapping from types to values."
  (let ((index 0)
	(length (length string))
	alist type value group)
    (while (< index length)
      (if (eq index (string-match "[ \t\n\r]*" string index))
	  (setq index (match-end 0)))
      (if (eq index (string-match
		     "\\([0-9]+\\(\\.[0-9]+\\)*\\)\[ \t\n\r]*=[ \t\n\r]*"
		     string index))
	  (setq type (match-string 1 string)
		index (match-end 0))
	(if (eq index (string-match "\\([0-9A-Za-z]+\\)[ \t\n\r]*=[ \t\n\r]*"
				    string index))
	    (setq type (match-string 1 string)
		  index (match-end 0))))
      (unless type
	(error "Invalid type"))
      (if (eq index (string-match
		     "\\([^,=+<>#;\\\"]\\|\\\\.\\)+"
		     string index))
	  (setq index (match-end 0)
		value (epg--decode-quotedstring (match-string 0 string)))
	(if (eq index (string-match "#\\([0-9A-Fa-f]+\\)" string index))
	    (setq index (match-end 0)
		  value (epg--decode-hexstring (match-string 1 string)))
	  (if (eq index (string-match "\"\\([^\\\"]\\|\\\\.\\)*\""
				      string index))
	      (setq index (match-end 0)
		    value (epg--decode-quotedstring
			   (match-string 0 string))))))
      (if group
	  (if (stringp (car (car alist)))
	      (setcar alist (list (cons type value) (car alist)))
	    (setcar alist (cons (cons type value) (car alist))))
	(if (consp (car (car alist)))
	    (setcar alist (nreverse (car alist))))
	(setq alist (cons (cons type value) alist)
	      type nil
	      value nil))
      (if (eq index (string-match "[ \t\n\r]*\\([,;+]\\)" string index))
	  (setq index (match-end 0)
		group (eq (aref string (match-beginning 1)) ?+))))
    (nreverse alist)))

(defun epg-decode-dn (alist)
  "Convert ALIST returned by `epg-dn-from-string' to a human readable form.
Type names are resolved using `epg-dn-type-alist'."
  (mapconcat
   (lambda (rdn)
     (if (stringp (car rdn))
	 (let ((entry (assoc (car rdn) epg-dn-type-alist)))
	   (if entry
	       (format "%s=%s" (cdr entry) (cdr rdn))
	     (format "%s=%s" (car rdn) (cdr rdn))))
       (concat "(" (epg-decode-dn rdn) ")")))
   alist
   ", "))

(provide 'epg)

;;; epg.el ends here
