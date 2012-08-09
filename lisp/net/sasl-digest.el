;;; sasl-digest.el --- DIGEST-MD5 module for the SASL client framework

;; Copyright (C) 2000, 2007-2012  Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;;	Kenichi OKADA <okada@opaopa.org>
;; Keywords: SASL, DIGEST-MD5
;; Package: sasl

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

;; This program is implemented from draft-leach-digest-sasl-05.txt.
;;
;; It is caller's responsibility to base64-decode challenges and
;; base64-encode responses in IMAP4 AUTHENTICATE command.
;;
;; Passphrase should be longer than 16 bytes. (See RFC 2195)

;;; Commentary:

(require 'sasl)
(require 'hmac-md5)

(defvar sasl-digest-md5-nonce-count 1)
(defvar sasl-digest-md5-unique-id-function
  sasl-unique-id-function)

(defvar sasl-digest-md5-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?, "." table)
    table)
  "A syntax table for parsing digest-challenge attributes.")

(defconst sasl-digest-md5-steps
  '(ignore				;no initial response
    sasl-digest-md5-response
    ignore))				;""

(defun sasl-digest-md5-parse-string (string)
  "Parse STRING and return a property list.
The value is a cons cell of the form \(realm nonce qop-options stale maxbuf
charset algorithm cipher-opts auth-param)."
  (with-temp-buffer
    (set-syntax-table sasl-digest-md5-syntax-table)
    (save-excursion
      (insert string)
      (goto-char (point-min))
      (insert "(")
      (while (progn (forward-sexp) (not (eobp)))
	(delete-char 1)
	(insert " "))
      (insert ")")
      (read (point-min-marker)))))

(defun sasl-digest-md5-digest-uri (serv-type host &optional serv-name)
  (concat serv-type "/" host
	  (if (and serv-name
		   (not (string= host serv-name)))
	      (concat "/" serv-name))))

(defun sasl-digest-md5-cnonce ()
  (let ((sasl-unique-id-function sasl-digest-md5-unique-id-function))
    (sasl-unique-id)))

(defun sasl-digest-md5-response-value (username
				       realm
				       nonce
				       cnonce
				       nonce-count
				       qop
				       digest-uri
				       authzid)
  (let ((passphrase
	 (sasl-read-passphrase
	  (format "DIGEST-MD5 passphrase for %s: "
		  username))))
    (unwind-protect
	(encode-hex-string
	 (md5-binary
	  (concat
	   (encode-hex-string
	    (md5-binary (concat (md5-binary
				 (concat username ":" realm ":" passphrase))
				":" nonce ":" cnonce
				(if authzid
				    (concat ":" authzid)))))
	   ":" nonce
	   ":" (format "%08x" nonce-count) ":" cnonce ":" qop ":"
	   (encode-hex-string
	    (md5-binary
	     (concat "AUTHENTICATE:" digest-uri
		     (if (member qop '("auth-int" "auth-conf"))
			 ":00000000000000000000000000000000")))))))
      (fillarray passphrase 0))))

(defun sasl-digest-md5-response (client step)
  (let* ((plist
	  (sasl-digest-md5-parse-string (sasl-step-data step)))
	 (realm
	  (or (sasl-client-property client 'realm)
	      (plist-get plist 'realm))) ;need to check
	 (nonce-count
	  (or (sasl-client-property client 'nonce-count)
	       sasl-digest-md5-nonce-count))
	 (qop
	  (or (sasl-client-property client 'qop)
	      "auth"))
	 (digest-uri
	  (sasl-digest-md5-digest-uri
	   (sasl-client-service client)(sasl-client-server client)))
	 (cnonce
	  (or (sasl-client-property client 'cnonce)
	      (sasl-digest-md5-cnonce))))
    (sasl-client-set-property client 'nonce-count (1+ nonce-count))
    (unless (string= qop "auth")
      (sasl-error (format "Unsupported \"qop-value\": %s" qop)))
    (concat
     "username=\"" (sasl-client-name client) "\","
     "realm=\"" realm "\","
     "nonce=\"" (plist-get plist 'nonce) "\","
     "cnonce=\"" cnonce "\","
     (format "nc=%08x," nonce-count)
     "digest-uri=\"" digest-uri "\","
     "qop=" qop ","
     "response="
     (sasl-digest-md5-response-value
      (sasl-client-name client)
      realm
      (plist-get plist 'nonce)
      cnonce
      nonce-count
      qop
      digest-uri
      (plist-get plist 'authzid)))))

(put 'sasl-digest 'sasl-mechanism
     (sasl-make-mechanism "DIGEST-MD5" sasl-digest-md5-steps))

(provide 'sasl-digest)

;;; sasl-digest.el ends here
