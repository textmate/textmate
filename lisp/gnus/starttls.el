;;; starttls.el --- STARTTLS functions

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Author: Simon Josefsson <simon@josefsson.org>
;; Created: 1999/11/20
;; Keywords: TLS, SSL, OpenSSL, GnuTLS, mail, news

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

;; This module defines some utility functions for STARTTLS profiles.

;; [RFC 2595] "Using TLS with IMAP, POP3 and ACAP"
;;	by Chris Newman <chris.newman@innosoft.com> (1999/06)

;; This file now contains a combination of the two previous
;; implementations both called "starttls.el".  The first one is Daiki
;; Ueno's starttls.el which uses his own "starttls" command line tool,
;; and the second one is Simon Josefsson's starttls.el which uses
;; "gnutls-cli" from GnuTLS.
;;
;; If "starttls" is available, it is preferred by the code over
;; "gnutls-cli", for backwards compatibility.  Use
;; `starttls-use-gnutls' to toggle between implementations if you have
;; both tools installed.  It is recommended to use GnuTLS, though, as
;; it performs more verification of the certificates.

;; The GnuTLS support requires GnuTLS 0.9.90 (released 2003-10-08) or
;; later, from <http://www.gnu.org/software/gnutls/>, or "starttls"
;; from <ftp://ftp.opaopa.org/pub/elisp/>.

;; Usage is similar to `open-network-stream'.  For example:
;;
;; (when (setq tmp (starttls-open-stream
;;			"test" (current-buffer) "yxa.extundo.com" 25))
;;   (accept-process-output tmp 15)
;;   (process-send-string tmp "STARTTLS\n")
;;   (accept-process-output tmp 15)
;;   (message "STARTTLS output:\n%s" (starttls-negotiate tmp))
;;   (process-send-string tmp "EHLO foo\n"))

;; An example run yields the following output:
;;
;; 220 yxa.extundo.com ESMTP Sendmail 8.12.11/8.12.11/Debian-3; Wed, 26 May 2004 19:12:29 +0200; (No UCE/UBE) logging access from: c494102a.s-bi.bostream.se(OK)-c494102a.s-bi.bostream.se [217.215.27.65]
;; 220 2.0.0 Ready to start TLS
;; 250-yxa.extundo.com Hello c494102a.s-bi.bostream.se [217.215.27.65], pleased to meet you
;; 250-ENHANCEDSTATUSCODES
;; 250-PIPELINING
;; 250-EXPN
;; 250-VERB
;; 250-8BITMIME
;; 250-SIZE
;; 250-DSN
;; 250-ETRN
;; 250-AUTH DIGEST-MD5 CRAM-MD5 PLAIN LOGIN
;; 250-DELIVERBY
;; 250 HELP
;; nil
;;
;; With the message buffer containing:
;;
;; STARTTLS output:
;; *** Starting TLS handshake
;; - Server's trusted authorities:
;;    [0]: C=SE,ST=Stockholm,L=Stockholm,O=YXA,OU=CA,CN=yxa.extundo.com,EMAIL=staff@yxa.extundo.com
;; - Certificate type: X.509
;;  - Got a certificate list of 2 certificates.
;;
;;  - Certificate[0] info:
;;  # The hostname in the certificate matches 'yxa.extundo.com'.
;;  # valid since: Wed May 26 12:16:00 CEST 2004
;;  # expires at: Wed Jul 26 12:16:00 CEST 2023
;;  # serial number: 04
;;  # fingerprint: 7c 04 4b c1 fa 26 9b 5d 90 22 52 3c 65 3d 85 3a
;;  # version: #1
;;  # public key algorithm: RSA
;;  #   Modulus: 1024 bits
;;  # Subject's DN: C=SE,ST=Stockholm,L=Stockholm,O=YXA,OU=Mail server,CN=yxa.extundo.com,EMAIL=staff@yxa.extundo.com
;;  # Issuer's DN: C=SE,ST=Stockholm,L=Stockholm,O=YXA,OU=CA,CN=yxa.extundo.com,EMAIL=staff@yxa.extundo.com
;;
;;  - Certificate[1] info:
;;  # valid since: Sun May 23 11:35:00 CEST 2004
;;  # expires at: Sun Jul 23 11:35:00 CEST 2023
;;  # serial number: 00
;;  # fingerprint: fc 76 d8 63 1a c9 0b 3b fa 40 fe ed 47 7a 58 ae
;;  # version: #3
;;  # public key algorithm: RSA
;;  #   Modulus: 1024 bits
;;  # Subject's DN: C=SE,ST=Stockholm,L=Stockholm,O=YXA,OU=CA,CN=yxa.extundo.com,EMAIL=staff@yxa.extundo.com
;;  # Issuer's DN: C=SE,ST=Stockholm,L=Stockholm,O=YXA,OU=CA,CN=yxa.extundo.com,EMAIL=staff@yxa.extundo.com
;;
;; - Peer's certificate issuer is unknown
;; - Peer's certificate is NOT trusted
;; - Version: TLS 1.0
;; - Key Exchange: RSA
;; - Cipher: ARCFOUR 128
;; - MAC: SHA
;; - Compression: NULL

;;; Code:

(defgroup starttls nil
  "Support for `Transport Layer Security' protocol."
  :version "21.1"
  :group 'mail)

(defcustom starttls-gnutls-program "gnutls-cli"
  "Name of GnuTLS command line tool.
This program is used when GnuTLS is used, i.e. when
`starttls-use-gnutls' is non-nil."
  :version "22.1"
  :type 'string
  :group 'starttls)

(defcustom starttls-program "starttls"
  "The program to run in a subprocess to open an TLSv1 connection.
This program is used when the `starttls' command is used,
i.e. when `starttls-use-gnutls' is nil."
  :type 'string
  :group 'starttls)

(defcustom starttls-use-gnutls (not (executable-find starttls-program))
  "*Whether to use GnuTLS instead of the `starttls' command."
  :version "22.1"
  :type 'boolean
  :group 'starttls)

(defcustom starttls-extra-args nil
  "Extra arguments to `starttls-program'.
These apply when the `starttls' command is used, i.e. when
`starttls-use-gnutls' is nil."
  :type '(repeat string)
  :group 'starttls)

(defcustom starttls-extra-arguments nil
  "Extra arguments to `starttls-program'.
These apply when GnuTLS is used, i.e. when `starttls-use-gnutls' is non-nil.

For example, non-TLS compliant servers may require
'(\"--protocols\" \"ssl3\").  Invoke \"gnutls-cli --help\" to
find out which parameters are available."
  :version "22.1"
  :type '(repeat string)
  :group 'starttls)

(defcustom starttls-process-connection-type nil
  "*Value for `process-connection-type' to use when starting STARTTLS process."
  :version "22.1"
  :type 'boolean
  :group 'starttls)

(defcustom starttls-connect "- Simple Client Mode:\n\n"
  "*Regular expression indicating successful connection.
The default is what GnuTLS's \"gnutls-cli\" outputs."
  ;; GnuTLS cli.c:main() prints this string when it is starting to run
  ;; in the application read/write phase.  If the logic, or the string
  ;; itself, is modified, this must be updated.
  :version "22.1"
  :type 'regexp
  :group 'starttls)

(defcustom starttls-failure "\\*\\*\\* Handshake has failed"
  "*Regular expression indicating failed TLS handshake.
The default is what GnuTLS's \"gnutls-cli\" outputs."
  ;; GnuTLS cli.c:do_handshake() prints this string on failure.  If the
  ;; logic, or the string itself, is modified, this must be updated.
  :version "22.1"
  :type 'regexp
  :group 'starttls)

(defcustom starttls-success "- Compression: "
  "*Regular expression indicating completed TLS handshakes.
The default is what GnuTLS's \"gnutls-cli\" outputs."
  ;; GnuTLS cli.c:do_handshake() calls, on success,
  ;; common.c:print_info(), that unconditionally print this string
  ;; last.  If that logic, or the string itself, is modified, this
  ;; must be updated.
  :version "22.1"
  :type 'regexp
  :group 'starttls)

(defun starttls-negotiate-gnutls (process)
  "Negotiate TLS on PROCESS opened by `open-starttls-stream'.
This should typically only be done once.  It typically returns a
multi-line informational message with information about the
handshake, or nil on failure."
  (let (buffer info old-max done-ok done-bad)
    (if (null (setq buffer (process-buffer process)))
	;; XXX How to remove/extract the TLS negotiation junk?
	(signal-process (process-id process) 'SIGALRM)
      (with-current-buffer buffer
	(save-excursion
	  (setq old-max (goto-char (point-max)))
	  (signal-process (process-id process) 'SIGALRM)
	  (while (and (processp process)
		      (eq (process-status process) 'run)
		      (save-excursion
			(goto-char old-max)
			(not (or (setq done-ok (re-search-forward
						starttls-success nil t))
				 (setq done-bad (re-search-forward
						 starttls-failure nil t))))))
	    (accept-process-output process 1 100)
	    (sit-for 0.1))
	  (setq info (buffer-substring-no-properties old-max (point-max)))
	  (delete-region old-max (point-max))
	  (if (or (and done-ok (not done-bad))
		  ;; Prevent mitm that fake success msg after failure msg.
		  (and done-ok done-bad (< done-ok done-bad)))
	      info
	    (message "STARTTLS negotiation failed: %s" info)
	    nil))))))

(defun starttls-negotiate (process)
  (if starttls-use-gnutls
      (starttls-negotiate-gnutls process)
    (signal-process (process-id process) 'SIGALRM)))

(eval-and-compile
  (if (fboundp 'set-process-query-on-exit-flag)
      (defalias 'starttls-set-process-query-on-exit-flag
	'set-process-query-on-exit-flag)
    (defalias 'starttls-set-process-query-on-exit-flag
      'process-kill-without-query)))

(defun starttls-open-stream-gnutls (name buffer host port)
  (message "Opening STARTTLS connection to `%s:%s'..." host port)
  (let* (done
	 (old-max (with-current-buffer buffer (point-max)))
	 (process-connection-type starttls-process-connection-type)
	 (process (apply #'start-process name buffer
			 starttls-gnutls-program "-s" host
			 "-p" (if (integerp port)
				  (int-to-string port)
				port)
			 starttls-extra-arguments)))
    (starttls-set-process-query-on-exit-flag process nil)
    (while (and (processp process)
		(eq (process-status process) 'run)
		(with-current-buffer buffer
		  (goto-char old-max)
		  (not (setq done (re-search-forward
				   starttls-connect nil t)))))
      (accept-process-output process 0 100)
      (sit-for 0.1))
    (if done
	(with-current-buffer buffer
	  (delete-region old-max done))
      (delete-process process)
      (setq process nil))
    (message "Opening STARTTLS connection to `%s:%s'...%s"
	     host port (if done "done" "failed"))
    process))

;;;###autoload
(defun starttls-open-stream (name buffer host port)
  "Open a TLS connection for a port to a host.
Returns a subprocess object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST PORT.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or `buffer-name') to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg PORT is an integer specifying a port to connect to.
If `starttls-use-gnutls' is nil, this may also be a service name, but
GnuTLS requires a port number."
  (if starttls-use-gnutls
      (starttls-open-stream-gnutls name buffer host port)
    (message "Opening STARTTLS connection to `%s:%s'" host (format "%s" port))
    (let* ((process-connection-type starttls-process-connection-type)
	   (process (apply #'start-process
			   name buffer starttls-program
			   host (format "%s" port)
			   starttls-extra-args)))
      (starttls-set-process-query-on-exit-flag process nil)
      process)))

(defun starttls-available-p ()
  "Say whether the STARTTLS programs are available."
  (and (not (memq system-type '(windows-nt ms-dos)))
       (executable-find (if starttls-use-gnutls
			    starttls-gnutls-program
			  starttls-program))))

(defalias 'starttls-any-program-available 'starttls-available-p)
(make-obsolete 'starttls-any-program-available 'starttls-available-p
	       "2011-08-02")

(provide 'starttls)

;;; starttls.el ends here
