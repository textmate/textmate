;;; tls.el --- TLS/SSL support via wrapper around GnuTLS

;; Copyright (C) 1996-1999, 2002-2012  Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: comm, tls, gnutls, ssl

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

;; This package implements a simple wrapper around "gnutls-cli" to
;; make Emacs support TLS/SSL.
;;
;; Usage is the same as `open-network-stream', i.e.:
;;
;; (setq tmp (open-tls-stream "test" (current-buffer) "news.mozilla.org" 563))
;; ...
;; #<process test>
;; (process-send-string tmp "mode reader\n")
;; 200 secnews.netscape.com Netscape-Collabra/3.52 03615 NNRP ready ...
;; nil
;; (process-send-string tmp "quit\n")
;; 205
;; nil

;; To use this package as a replacement for ssl.el by William M. Perry
;; <wmperry@cs.indiana.edu>, you need to evaluate the following:
;;
;; (defalias 'open-ssl-stream 'open-tls-stream)

;;; Code:

(autoload 'format-spec "format-spec")
(autoload 'format-spec-make "format-spec")

(defgroup tls nil
  "Transport Layer Security (TLS) parameters."
  :group 'comm)

(defcustom tls-end-of-info
  (concat
   "\\("
   ;; `openssl s_client' regexp.  See ssl/ssl_txt.c lines 219-220.
   ;; According to apps/s_client.c line 1515 `---' is always the last
   ;; line that is printed by s_client before the real data.
   "^    Verify return code: .+\n---\n\\|"
   ;; `gnutls' regexp. See src/cli.c lines 721-.
   "^- Simple Client Mode:\n"
   "\\(\n\\|"                           ; ignore blank lines
   ;; According to GnuTLS v2.1.5 src/cli.c lines 640-650 and 705-715
   ;; in `main' the handshake will start after this message.  If the
   ;; handshake fails, the programs will abort.
   "^\\*\\*\\* Starting TLS handshake\n\\)*"
   "\\)")
  "Regexp matching end of TLS client informational messages.
Client data stream begins after the last character matched by
this.  The default matches `openssl s_client' (version 0.9.8c)
and `gnutls-cli' (version 2.0.1) output."
  :version "22.2"
  :type 'regexp
  :group 'tls)

(defcustom tls-program '("gnutls-cli --insecure -p %p %h"
			 "gnutls-cli --insecure -p %p %h --protocols ssl3"
			 "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
  "List of strings containing commands to start TLS stream to a host.
Each entry in the list is tried until a connection is successful.
%h is replaced with server hostname, %p with port to connect to.
The program should read input on stdin and write output to
stdout.

See `tls-checktrust' on how to check trusted root certs.

Also see `tls-success' for what the program should output after
successful negotiation."
  :type
  '(choice
    (list :tag "Choose commands"
	  :value
	  ("gnutls-cli -p %p %h"
	   "gnutls-cli -p %p %h --protocols ssl3"
	   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
	  (set :inline t
	       ;; FIXME: add brief `:tag "..."' descriptions.
	       ;; (repeat :inline t :tag "Other" (string))
	       ;; See `tls-checktrust':
	       (const "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h")
	       (const "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3")
	       (const "openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof")
	       ;; No trust check:
	       (const "gnutls-cli -p %p %h")
	       (const "gnutls-cli -p %p %h --protocols ssl3")
	       (const "openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))
	  (repeat :inline t :tag "Other" (string)))
    (const :tag "Default list of commands"
	   ("gnutls-cli -p %p %h"
	    "gnutls-cli -p %p %h --protocols ssl3"
	    "openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))
    (list :tag "List of commands"
	  (repeat :tag "Command" (string))))
  :version "22.1"
  :group 'tls)

(defcustom tls-process-connection-type nil
  "Value for `process-connection-type' to use when starting TLS process."
  :version "22.1"
  :type 'boolean
  :group 'tls)

(defcustom tls-success "- Handshake was completed\\|SSL handshake has read "
  "Regular expression indicating completed TLS handshakes.
The default is what GnuTLS's \"gnutls-cli\" or OpenSSL's
\"openssl s_client\" outputs."
  :version "22.1"
  :type 'regexp
  :group 'tls)

(defcustom tls-checktrust nil
  "Indicate if certificates should be checked against trusted root certs.
If this is `ask', the user can decide whether to accept an
untrusted certificate.  You may have to adapt `tls-program' in
order to make this feature work properly, i.e., to ensure that
the external program knows about the root certificates you
consider trustworthy, e.g.:

\(setq tls-program
      '(\"gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h\"
	\"gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3\"
	\"openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof\"))"
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask))
  :version "23.1" ;; No Gnus
  :group 'tls)

(defcustom tls-untrusted
  "- Peer's certificate is NOT trusted\\|Verify return code: \\([^0] \\|.[^ ]\\)"
  "Regular expression indicating failure of TLS certificate verification.
The default is what GnuTLS's \"gnutls-cli\" or OpenSSL's
\"openssl s_client\" return in the event of unsuccessful
verification."
  :type 'regexp
  :version "23.1" ;; No Gnus
  :group 'tls)

(defcustom tls-hostmismatch
  "# The hostname in the certificate does NOT match"
  "Regular expression indicating a host name mismatch in certificate.
When the host name specified in the certificate doesn't match the
name of the host you are connecting to, gnutls-cli issues a
warning to this effect.  There is no such feature in openssl.  Set
this to nil if you want to ignore host name mismatches."
  :type 'regexp
  :version "23.1" ;; No Gnus
  :group 'tls)

(defcustom tls-certtool-program (executable-find "certtool")
  "Name of  GnuTLS certtool.
Used by `tls-certificate-information'."
  :version "22.1"
  :type 'string
  :group 'tls)

(defun tls-certificate-information (der)
  "Parse X.509 certificate in DER format into an assoc list."
  (let ((certificate (concat "-----BEGIN CERTIFICATE-----\n"
			     (base64-encode-string der)
			     "\n-----END CERTIFICATE-----\n"))
	(exit-code 0))
    (with-current-buffer (get-buffer-create " *certtool*")
      (erase-buffer)
      (insert certificate)
      (setq exit-code (condition-case ()
			  (call-process-region (point-min) (point-max)
					       tls-certtool-program
					       t (list (current-buffer) nil) t
					       "--certificate-info")
			(error -1)))
      (if (/= exit-code 0)
	  nil
	(let ((vals nil))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\([^:]+\\): \\(.*\\)" nil t)
	    (push (cons (match-string 1) (match-string 2)) vals))
	  (nreverse vals))))))

(defun open-tls-stream (name buffer host port)
  "Open a TLS connection for a port to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST PORT.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg PORT is an integer specifying a port to connect to."
  (let ((cmds tls-program)
	(use-temp-buffer (null buffer))
	process	cmd done)
    (if use-temp-buffer
	(setq buffer (generate-new-buffer " TLS"))
      ;; BUFFER is a string but does not exist as a buffer object.
      (unless (and (get-buffer buffer)
		   (buffer-name (get-buffer buffer)))
	(generate-new-buffer buffer)))
    (with-current-buffer buffer
      (message "Opening TLS connection to `%s'..." host)
      (while (and (not done) (setq cmd (pop cmds)))
	(let ((process-connection-type tls-process-connection-type)
	      (formatted-cmd
	       (format-spec
		cmd
		(format-spec-make
		 ?h host
		 ?p (if (integerp port)
			(int-to-string port)
		      port))))
	      response)
	  (message "Opening TLS connection with `%s'..." formatted-cmd)
	  (setq process (start-process
			 name buffer shell-file-name shell-command-switch
			 formatted-cmd))
	  (while (and process
		      (memq (process-status process) '(open run))
		      (progn
			(goto-char (point-min))
			(not (setq done (re-search-forward
					 tls-success nil t)))))
	    (unless (accept-process-output process 1)
	      (sit-for 1)))
	  (message "Opening TLS connection with `%s'...%s" formatted-cmd
		   (if done "done" "failed"))
	  (if (not done)
	      (delete-process process)
	    ;; advance point to after all informational messages that
	    ;; `openssl s_client' and `gnutls' print
	    (let ((start-of-data nil))
	      (while
		  (not (setq start-of-data
			     ;; the string matching `tls-end-of-info'
			     ;; might come in separate chunks from
			     ;; `accept-process-output', so start the
			     ;; search where `tls-success' ended
			     (save-excursion
			       (if (re-search-forward tls-end-of-info nil t)
				   (match-end 0)))))
		(accept-process-output process 1))
	      (if start-of-data
		  ;; move point to start of client data
		  (goto-char start-of-data)))
	    (setq done process))))
      (when (and done
		 (or
		  (and tls-checktrust
		       (save-excursion
			 (goto-char (point-min))
			 (re-search-forward tls-untrusted nil t))
		       (or
			(and (not (eq tls-checktrust 'ask))
			     (message "The certificate presented by `%s' is \
NOT trusted." host))
			(not (yes-or-no-p
			      (format "The certificate presented by `%s' is \
NOT trusted. Accept anyway? " host)))))
		  (and tls-hostmismatch
		       (save-excursion
			 (goto-char (point-min))
			 (re-search-forward tls-hostmismatch nil t))
		       (not (yes-or-no-p
			     (format "Host name in certificate doesn't \
match `%s'. Connect anyway? " host))))))
	(setq done nil)
	(delete-process process)))
    (message "Opening TLS connection to `%s'...%s"
	     host (if done "done" "failed"))
    (when use-temp-buffer
      (if done (set-process-buffer process nil))
      (kill-buffer buffer))
    done))

(provide 'tls)

;;; tls.el ends here
