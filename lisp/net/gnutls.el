;;; gnutls.el --- Support SSL/TLS connections through GnuTLS

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: comm, tls, ssl, encryption
;; Originally-By: Simon Josefsson (See http://josefsson.org/emacs-security/)
;; Thanks-To: Lars Magne Ingebrigtsen <larsi@gnus.org>

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

;; This package provides language bindings for the GnuTLS library
;; using the corresponding core functions in gnutls.c.  It should NOT
;; be used directly, only through open-protocol-stream.

;; Simple test:
;;
;; (open-gnutls-stream "tls" "tls-buffer" "yourserver.com" "https")
;; (open-gnutls-stream "tls" "tls-buffer" "imap.gmail.com" "imaps")

;;; Code:

(eval-when-compile (require 'cl))

(defgroup gnutls nil
  "Emacs interface to the GnuTLS library."
  :version "24.1"
  :prefix "gnutls-"
  :group 'net-utils)

(defcustom gnutls-algorithm-priority nil
  "If non-nil, this should be a TLS priority string.
For instance, if you want to skip the \"dhe-rsa\" algorithm,
set this variable to \"normal:-dhe-rsa\"."
  :group 'gnutls
  :type '(choice (const nil)
                 string))

(defcustom gnutls-trustfiles
  '(
    "/etc/ssl/certs/ca-certificates.crt" ; Debian, Ubuntu, Gentoo and Arch Linux
    "/etc/pki/tls/certs/ca-bundle.crt"   ; Fedora and RHEL
    "/etc/ssl/ca-bundle.pem"             ; Suse
    "/usr/ssl/certs/ca-bundle.crt"       ; Cygwin
    )
  "List of CA bundle location filenames or a function returning said list.
The files may be in PEM or DER format, as per the GnuTLS documentation.
The files may not exist, in which case they will be ignored."
  :group 'gnutls
  :type '(choice (function :tag "Function to produce list of bundle filenames")
                 (repeat (file :tag "Bundle filename"))))

;;;###autoload
(defcustom gnutls-min-prime-bits 256
  ;; Several mail servers send fewer bits than the GnuTLS default.
  ;; Currently, 256 appears to be a reasonable choice (Bug#11267).
  "Minimum number of prime bits accepted by GnuTLS for key exchange.
During a Diffie-Hellman handshake, if the server sends a prime
number with fewer than this number of bits, the handshake is
rejected.  \(The smaller the prime number, the less secure the
key exchange is against man-in-the-middle attacks.)

A value of nil says to use the default GnuTLS value."
  :type '(choice (const :tag "Use default value" nil)
                 (integer :tag "Number of bits" 512))
  :group 'gnutls)

(defun open-gnutls-stream (name buffer host service)
  "Open a SSL/TLS connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or `buffer-name') to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to.

Usage example:

  \(with-temp-buffer
    \(open-gnutls-stream \"tls\"
                        \(current-buffer)
                        \"your server goes here\"
                        \"imaps\"))

This is a very simple wrapper around `gnutls-negotiate'.  See its
documentation for the specific parameters you can use to open a
GnuTLS connection, including specifying the credential type,
trust and key files, and priority string."
  (gnutls-negotiate :process (open-network-stream name buffer host service)
                    :type 'gnutls-x509pki
                    :hostname host))

(put 'gnutls-error
     'error-conditions
     '(error gnutls-error))
(put 'gnutls-error
     'error-message "GnuTLS error")

(declare-function gnutls-boot "gnutls.c" (proc type proplist))
(declare-function gnutls-errorp "gnutls.c" (error))

(defun* gnutls-negotiate
    (&rest spec
           &key process type hostname priority-string
           trustfiles crlfiles keylist min-prime-bits
           verify-flags verify-error verify-hostname-error
           &allow-other-keys)
  "Negotiate a SSL/TLS connection.  Returns proc.  Signals gnutls-error.

Note arguments are passed CL style, :type TYPE instead of just TYPE.

TYPE is `gnutls-x509pki' (default) or `gnutls-anon'.  Use nil for the default.
PROCESS is a process returned by `open-network-stream'.
HOSTNAME is the remote hostname.  It must be a valid string.
PRIORITY-STRING is as per the GnuTLS docs, default is \"NORMAL\".
TRUSTFILES is a list of CA bundles.  It defaults to `gnutls-trustfiles'.
CRLFILES is a list of CRL files.
KEYLIST is an alist of (client key file, client cert file) pairs.
MIN-PRIME-BITS is the minimum acceptable size of Diffie-Hellman keys
\(see `gnutls-min-prime-bits' for more information).  Use nil for the
default.

When VERIFY-HOSTNAME-ERROR is not nil, an error will be raised
when the hostname does not match the presented certificate's host
name.  The exact verification algorithm is a basic implementation
of the matching described in RFC2818 (HTTPS), which takes into
account wildcards, and the DNSName/IPAddress subject alternative
name PKIX extension.  See GnuTLS' gnutls_x509_crt_check_hostname
for details.  When VERIFY-HOSTNAME-ERROR is nil, only a warning
will be issued.

When VERIFY-ERROR is not nil, an error will be raised when the
peer certificate verification fails as per GnuTLS'
gnutls_certificate_verify_peers2.  Otherwise, only warnings will
be shown about the verification failure.

VERIFY-FLAGS is a numeric OR of verification flags only for
`gnutls-x509pki' connections.  See GnuTLS' x509.h for details;
here's a recent version of the list.

    GNUTLS_VERIFY_DISABLE_CA_SIGN = 1,
    GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT = 2,
    GNUTLS_VERIFY_DO_NOT_ALLOW_SAME = 4,
    GNUTLS_VERIFY_ALLOW_ANY_X509_V1_CA_CRT = 8,
    GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD2 = 16,
    GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD5 = 32,
    GNUTLS_VERIFY_DISABLE_TIME_CHECKS = 64,
    GNUTLS_VERIFY_DISABLE_TRUSTED_TIME_CHECKS = 128,
    GNUTLS_VERIFY_DO_NOT_ALLOW_X509_V1_CA_CRT = 256

It must be omitted, a number, or nil; if omitted or nil it
defaults to GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT."
  (let* ((type (or type 'gnutls-x509pki))
         (trustfiles (or trustfiles
                         (delq nil
                               (mapcar (lambda (f) (and f (file-exists-p f) f))
                                       (if (functionp gnutls-trustfiles)
                                           (funcall gnutls-trustfiles)
                                         gnutls-trustfiles)))))
         (priority-string (or priority-string
                              (cond
                               ((eq type 'gnutls-anon)
                                "NORMAL:+ANON-DH:!ARCFOUR-128")
                               ((eq type 'gnutls-x509pki)
                                (if gnutls-algorithm-priority
                                    (upcase gnutls-algorithm-priority)
                                  "NORMAL")))))
         (min-prime-bits (or min-prime-bits gnutls-min-prime-bits))
         (params `(:priority ,priority-string
                             :hostname ,hostname
                             :loglevel ,gnutls-log-level
                             :min-prime-bits ,min-prime-bits
                             :trustfiles ,trustfiles
                             :crlfiles ,crlfiles
                             :keylist ,keylist
                             :verify-flags ,verify-flags
                             :verify-error ,verify-error
                             :verify-hostname-error ,verify-hostname-error
                             :callbacks nil))
         ret)

    (gnutls-message-maybe
     (setq ret (gnutls-boot process type params))
     "boot: %s" params)

    (when (gnutls-errorp ret)
      ;; This is a error from the underlying C code.
      (signal 'gnutls-error (list process ret)))

    process))

(declare-function gnutls-error-string "gnutls.c" (error))

(defun gnutls-message-maybe (doit format &rest params)
  "When DOIT, message with the caller name followed by FORMAT on PARAMS."
  ;; (apply 'debug format (or params '(nil)))
  (when (gnutls-errorp doit)
    (message "%s: (err=[%s] %s) %s"
             "gnutls.el"
             doit (gnutls-error-string doit)
             (apply 'format format (or params '(nil))))))

(provide 'gnutls)

;;; gnutls.el ends here
