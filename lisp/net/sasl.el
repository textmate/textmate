;;; sasl.el --- SASL client framework

;; Copyright (C) 2000, 2007-2012  Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: SASL

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

;; This module provides common interface functions to share several
;; SASL mechanism drivers.  The toplevel is designed to be mostly
;; compatible with [Java-SASL].
;;
;; [SASL] J. Myers, "Simple Authentication and Security Layer (SASL)",
;;	RFC 2222, October 1997.
;;
;; [Java-SASL] R. Weltman & R. Lee, "The Java SASL Application Program
;;	Interface", draft-weltman-java-sasl-03.txt, March 2000.

;;; Code:

(defvar sasl-mechanisms
  '("CRAM-MD5" "DIGEST-MD5" "PLAIN" "LOGIN" "ANONYMOUS"
    "NTLM" "SCRAM-MD5"))

(defvar sasl-mechanism-alist
  '(("CRAM-MD5" sasl-cram)
    ("DIGEST-MD5" sasl-digest)
    ("PLAIN" sasl-plain)
    ("LOGIN" sasl-login)
    ("ANONYMOUS" sasl-anonymous)
    ("NTLM" sasl-ntlm)
    ("SCRAM-MD5" sasl-scram)))

(defvar sasl-unique-id-function #'sasl-unique-id-function)

(put 'sasl-error 'error-message "SASL error")
(put 'sasl-error 'error-conditions '(sasl-error error))

(defun sasl-error (datum)
  (signal 'sasl-error (list datum)))

;;; @ SASL client
;;;

(defun sasl-make-client (mechanism name service server)
  "Return a newly allocated SASL client.
NAME is name of the authorization.  SERVICE is name of the service desired.
SERVER is the fully qualified host name of the server to authenticate to."
  (vector mechanism name service server (make-symbol "sasl-client-properties")))

(defun sasl-client-mechanism (client)
  "Return the authentication mechanism driver of CLIENT."
  (aref client 0))

(defun sasl-client-name (client)
  "Return the authorization name of CLIENT, a string."
  (aref client 1))

(defun sasl-client-service (client)
  "Return the service name of CLIENT, a string."
  (aref client 2))

(defun sasl-client-server (client)
  "Return the server name of CLIENT, a string."
  (aref client 3))

(defun sasl-client-set-properties (client plist)
  "Destructively set the properties of CLIENT.
The second argument PLIST is the new property list."
  (setplist (aref client 4) plist))

(defun sasl-client-set-property (client property value)
  "Add the given PROPERTY/VALUE to CLIENT."
  (put (aref client 4) property value))

(defun sasl-client-property (client property)
  "Return the value of the PROPERTY of CLIENT."
  (get (aref client 4) property))

(defun sasl-client-properties (client)
  "Return the properties of CLIENT."
  (symbol-plist (aref client 4)))

;;; @ SASL mechanism
;;;

(defun sasl-make-mechanism (name steps)
  "Make an authentication mechanism.
NAME is a IANA registered SASL mechanism name.
STEPS is list of continuation functions."
  (vector name
	  (mapcar
	   (lambda (step)
	     (let ((symbol (make-symbol (symbol-name step))))
	       (fset symbol (symbol-function step))
	       symbol))
	   steps)))

(defun sasl-mechanism-name (mechanism)
  "Return name of MECHANISM, a string."
  (aref mechanism 0))

(defun sasl-mechanism-steps (mechanism)
  "Return the authentication steps of MECHANISM, a list of functions."
  (aref mechanism 1))

(defun sasl-find-mechanism (mechanisms)
  "Retrieve an appropriate mechanism object from MECHANISMS hints."
  (let* ((sasl-mechanisms sasl-mechanisms)
	 (mechanism
	  (catch 'done
	    (while sasl-mechanisms
	      (if (member (car sasl-mechanisms) mechanisms)
		  (throw 'done (nth 1 (assoc (car sasl-mechanisms)
					     sasl-mechanism-alist))))
	      (setq sasl-mechanisms (cdr sasl-mechanisms))))))
    (if mechanism
	(require mechanism))
    (get mechanism 'sasl-mechanism)))

;;; @ SASL authentication step
;;;

(defun sasl-step-data (step)
  "Return the data which STEP holds, a string."
  (aref step 1))

(defun sasl-step-set-data (step data)
  "Store DATA string to STEP."
  (aset step 1 data))

(defun sasl-next-step (client step)
  "Evaluate the challenge and prepare an appropriate next response.
The data type of the value and 2nd argument STEP is nil or opaque
authentication step which holds the reference to the next action and
the current challenge.  At the first time STEP should be set to nil."
  (let* ((steps
	  (sasl-mechanism-steps
	   (sasl-client-mechanism client)))
	 (function
	  (if (vectorp step)
	      (nth 1 (memq (aref step 0) steps))
	    (car steps))))
    (if function
	(vector function (funcall function client step)))))

(defvar sasl-read-passphrase nil)
(defun sasl-read-passphrase (prompt)
  (if (not sasl-read-passphrase)
      (if (functionp 'read-passwd)
	  (setq sasl-read-passphrase 'read-passwd)
	(if (load "passwd" t)
	    (setq sasl-read-passphrase 'read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp")
	  (setq sasl-read-passphrase 'ange-ftp-read-passwd))))
  (funcall sasl-read-passphrase prompt))

(defun sasl-unique-id ()
  "Compute a data string which must be different each time.
It contain at least 64 bits of entropy."
  (concat (funcall sasl-unique-id-function)(funcall sasl-unique-id-function)))

(defvar sasl-unique-id-char nil)

;; stolen (and renamed) from message.el
(defun sasl-unique-id-function ()
  ;; Don't use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq sasl-unique-id-char
	(% (1+ (or sasl-unique-id-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (current-time)))
    (concat
     (sasl-unique-id-number-base36
      (+ (car   tm)
	 (lsh (% sasl-unique-id-char 25) 16)) 4)
     (sasl-unique-id-number-base36
      (+ (nth 1 tm)
	 (lsh (/ sasl-unique-id-char 25) 16)) 4))))

(defun sasl-unique-id-number-base36 (num len)
  (if (if (< len 0)
	  (<= num 0)
	(= len 0))
      ""
    (concat (sasl-unique-id-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))

;;; PLAIN (RFC2595 Section 6)
(defconst sasl-plain-steps
  '(sasl-plain-response))

(defun sasl-plain-response (client step)
  (let ((passphrase
	 (sasl-read-passphrase
	  (format "PLAIN passphrase for %s: " (sasl-client-name client))))
	(authenticator-name
	 (sasl-client-property
	  client 'authenticator-name))
	(name (sasl-client-name client)))
    (unwind-protect
	(if (and authenticator-name
		 (not (string= authenticator-name name)))
	    (concat authenticator-name "\0" name "\0" passphrase)
	  (concat "\0" name "\0" passphrase))
      (fillarray passphrase 0))))

(put 'sasl-plain 'sasl-mechanism
     (sasl-make-mechanism "PLAIN" sasl-plain-steps))

(provide 'sasl-plain)

;;; LOGIN (No specification exists)
(defconst sasl-login-steps
  '(ignore				;no initial response
    sasl-login-response-1
    sasl-login-response-2))

(defun sasl-login-response-1 (client step)
;;;  (unless (string-match "^Username:" (sasl-step-data step))
;;;    (sasl-error (format "Unexpected response: %s" (sasl-step-data step))))
  (sasl-client-name client))

(defun sasl-login-response-2 (client step)
;;;  (unless (string-match "^Password:" (sasl-step-data step))
;;;    (sasl-error (format "Unexpected response: %s" (sasl-step-data step))))
  (sasl-read-passphrase
   (format "LOGIN passphrase for %s: " (sasl-client-name client))))

(put 'sasl-login 'sasl-mechanism
     (sasl-make-mechanism "LOGIN" sasl-login-steps))

(provide 'sasl-login)

;;; ANONYMOUS (RFC2245)
(defconst sasl-anonymous-steps
  '(ignore				;no initial response
    sasl-anonymous-response))

(defun sasl-anonymous-response (client step)
  (or (sasl-client-property client 'trace)
      (sasl-client-name client)))

(put 'sasl-anonymous 'sasl-mechanism
     (sasl-make-mechanism "ANONYMOUS" sasl-anonymous-steps))

(provide 'sasl-anonymous)

(provide 'sasl)

;;; sasl.el ends here
