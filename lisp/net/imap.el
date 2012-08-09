;;; imap.el --- imap library

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: mail

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

;; imap.el is an elisp library providing an interface for talking to
;; IMAP servers.
;;
;; imap.el is roughly divided in two parts, one that parses IMAP
;; responses from the server and storing data into buffer-local
;; variables, and one for utility functions which send commands to
;; server, waits for an answer, and return information.  The latter
;; part is layered on top of the previous.
;;
;; The imap.el API consist of the following functions, other functions
;; in this file should not be called directly and the result of doing
;; so are at best undefined.
;;
;; Global commands:
;;
;; imap-open,       imap-opened,    imap-authenticate, imap-close,
;; imap-capability, imap-namespace, imap-error-text
;;
;; Mailbox commands:
;;
;; imap-mailbox-get,       imap-mailbox-map,         imap-current-mailbox,
;; imap-current-mailbox-p, imap-search,              imap-mailbox-select,
;; imap-mailbox-examine,   imap-mailbox-unselect,    imap-mailbox-expunge
;; imap-mailbox-close,     imap-mailbox-create,      imap-mailbox-delete
;; imap-mailbox-rename,    imap-mailbox-lsub,        imap-mailbox-list
;; imap-mailbox-subscribe, imap-mailbox-unsubscribe, imap-mailbox-status
;; imap-mailbox-acl-get,   imap-mailbox-acl-set,     imap-mailbox-acl-delete
;;
;; Message commands:
;;
;; imap-fetch-asynch,                 imap-fetch,
;; imap-current-message,              imap-list-to-message-set,
;; imap-message-get,                  imap-message-map
;; imap-message-envelope-date,        imap-message-envelope-subject,
;; imap-message-envelope-from,        imap-message-envelope-sender,
;; imap-message-envelope-reply-to,    imap-message-envelope-to,
;; imap-message-envelope-cc,          imap-message-envelope-bcc
;; imap-message-envelope-in-reply-to, imap-message-envelope-message-id
;; imap-message-body,                 imap-message-flag-permanent-p
;; imap-message-flags-set,            imap-message-flags-del
;; imap-message-flags-add,            imap-message-copyuid
;; imap-message-copy,                 imap-message-appenduid
;; imap-message-append,               imap-envelope-from
;; imap-body-lines
;;
;; It is my hope that these commands should be pretty self
;; explanatory for someone that know IMAP.  All functions have
;; additional documentation on how to invoke them.
;;
;; imap.el supports RFC1730/2060/RFC3501 (IMAP4/IMAP4rev1).  The implemented
;; IMAP extensions are RFC2195 (CRAM-MD5), RFC2086 (ACL), RFC2342
;; (NAMESPACE), RFC2359 (UIDPLUS), the IMAP-part of RFC2595 (STARTTLS,
;; LOGINDISABLED) (with use of external library starttls.el and
;; program starttls), and the GSSAPI / Kerberos V4 sections of RFC1731
;; (with use of external program `imtest'), and RFC2971 (ID).  It also
;; takes advantage of the UNSELECT extension in Cyrus IMAPD.
;;
;; Without the work of John McClary Prevost and Jim Radford this library
;; would not have seen the light of day.  Many thanks.
;;
;; This is a transcript of a short interactive session for demonstration
;; purposes.
;;
;; (imap-open "my.mail.server")
;; => " *imap* my.mail.server:0"
;;
;; The rest are invoked with current buffer as the buffer returned by
;; `imap-open'.  It is possible to do it all without this, but it would
;; look ugly here since `buffer' is always the last argument for all
;; imap.el API functions.
;;
;; (imap-authenticate "myusername" "mypassword")
;; => auth
;;
;; (imap-mailbox-lsub "*")
;; => ("INBOX.sentmail" "INBOX.private" "INBOX.draft" "INBOX.spam")
;;
;; (imap-mailbox-list "INBOX.n%")
;; => ("INBOX.namedroppers" "INBOX.nnimap" "INBOX.ntbugtraq")
;;
;; (imap-mailbox-select "INBOX.nnimap")
;; => "INBOX.nnimap"
;;
;; (imap-mailbox-get 'exists)
;; => 166
;;
;; (imap-mailbox-get 'uidvalidity)
;; => "908992622"
;;
;; (imap-search "FLAGGED SINCE 18-DEC-98")
;; => (235 236)
;;
;; (imap-fetch 235 "RFC822.PEEK" 'RFC822)
;; => "X-Sieve: cmu-sieve 1.3^M\nX-Username: <jas@pdc.kth.se>^M\r...."
;;
;; Todo:
;;
;; o Parse UIDs as strings? We need to overcome the 28 bit limit somehow.
;;   Use IEEE floats (which are effectively exact)?  -- fx
;; o Don't use `read' at all (important places already fixed)
;; o Accept list of articles instead of message set string in most
;;   imap-message-* functions.
;; o Send strings as literal if they contain, e.g., ".
;;
;; Revision history:
;;
;;  - 19991218 added starttls/digest-md5 patch,
;;             by Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;;             NB! you need SLIM for starttls.el and digest-md5.el
;;  - 19991023 committed to pgnus
;;

;;; Code:

(eval-when-compile (require 'cl))
(eval-and-compile
  ;; For Emacs <22.2 and XEmacs.
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r)))
  (autoload 'starttls-open-stream "starttls")
  (autoload 'starttls-negotiate "starttls")
  (autoload 'sasl-find-mechanism "sasl")
  (autoload 'digest-md5-parse-digest-challenge "digest-md5")
  (autoload 'digest-md5-digest-response "digest-md5")
  (autoload 'digest-md5-digest-uri "digest-md5")
  (autoload 'digest-md5-challenge "digest-md5")
  (autoload 'rfc2104-hash "rfc2104")
  (autoload 'utf7-encode "utf7")
  (autoload 'utf7-decode "utf7")
  (autoload 'format-spec "format-spec")
  (autoload 'format-spec-make "format-spec")
  (autoload 'open-tls-stream "tls"))

;; User variables.

(defgroup imap nil
  "Low-level IMAP issues."
  :version "21.1"
  :group 'mail)

(defcustom imap-kerberos4-program '("imtest -m kerberos_v4 -u %l -p %p %s"
				    "imtest -kp %s %p")
  "List of strings containing commands for Kerberos 4 authentication.
%s is replaced with server hostname, %p with port to connect to, and
%l with the value of `imap-default-user'.  The program should accept
IMAP commands on stdin and return responses to stdout.  Each entry in
the list is tried until a successful connection is made."
  :group 'imap
  :type '(repeat string))

(defcustom imap-gssapi-program (list
				(concat "gsasl %s %p "
					"--mechanism GSSAPI "
					"--authentication-id %l")
				"imtest -m gssapi -u %l -p %p %s")
  "List of strings containing commands for GSSAPI (krb5) authentication.
%s is replaced with server hostname, %p with port to connect to, and
%l with the value of `imap-default-user'.  The program should accept
IMAP commands on stdin and return responses to stdout.  Each entry in
the list is tried until a successful connection is made."
  :group 'imap
  :type '(repeat string))

(defcustom imap-ssl-program '("openssl s_client -quiet -ssl3 -connect %s:%p"
			      "openssl s_client -quiet -ssl2 -connect %s:%p"
			      "s_client -quiet -ssl3 -connect %s:%p"
			      "s_client -quiet -ssl2 -connect %s:%p")
  "A string, or list of strings, containing commands for SSL connections.
Within a string, %s is replaced with the server address and %p with
port number on server.  The program should accept IMAP commands on
stdin and return responses to stdout.  Each entry in the list is tried
until a successful connection is made."
  :group 'imap
  :type '(choice string
		 (repeat string)))

(defcustom imap-shell-program '("ssh %s imapd"
				"rsh %s imapd"
				"ssh %g ssh %s imapd"
				"rsh %g rsh %s imapd")
  "A list of strings, containing commands for IMAP connection.
Within a string, %s is replaced with the server address, %p with port
number on server, %g with `imap-shell-host', and %l with
`imap-default-user'.  The program should read IMAP commands from stdin
and write IMAP response to stdout.  Each entry in the list is tried
until a successful connection is made."
  :group 'imap
  :type '(repeat string))

(defcustom imap-process-connection-type nil
  "*Value for `process-connection-type' to use for Kerberos4, GSSAPI, shell, and SSL.
The `process-connection-type' variable controls the type of device
used to communicate with subprocesses.  Values are nil to use a
pipe, or t or `pty' to use a pty.  The value has no effect if the
system has no ptys or if all ptys are busy: then a pipe is used
in any case.  The value takes effect when an IMAP server is
opened; changing it after that has no effect."
  :version "22.1"
  :group 'imap
  :type 'boolean)

(defcustom imap-use-utf7 t
  "If non-nil, do utf7 encoding/decoding of mailbox names.
Since the UTF7 decoding currently only decodes into ISO-8859-1
characters, you may disable this decoding if you need to access UTF7
encoded mailboxes which doesn't translate into ISO-8859-1."
  :group 'imap
  :type 'boolean)

(defcustom imap-log nil
  "If non-nil, an imap session trace is placed in `imap-log-buffer'.
Note that username, passwords and other privacy sensitive
information (such as e-mail) may be stored in the buffer.
It is not written to disk, however.  Do not enable this
variable unless you are comfortable with that.

See also `imap-debug'."
  :group 'imap
  :type 'boolean)

(defcustom imap-debug nil
  "If non-nil, trace imap- functions into `imap-debug-buffer'.
Uses `trace-function-background', so you can turn it off with,
say, `untrace-all'.

Note that username, passwords and other privacy sensitive
information (such as e-mail) may be stored in the buffer.
It is not written to disk, however.  Do not enable this
variable unless you are comfortable with that.

This variable only takes effect when loading the `imap' library.
See also `imap-log'."
  :group 'imap
  :type 'boolean)

(defcustom imap-shell-host "gateway"
  "Hostname of rlogin proxy."
  :group 'imap
  :type 'string)

(defcustom imap-default-user (user-login-name)
  "Default username to use."
  :group 'imap
  :type 'string)

(defcustom imap-read-timeout (if (string-match
				  "windows-nt\\|os/2\\|cygwin"
				  (symbol-name system-type))
				 1.0
			       0.1)
  "*How long to wait between checking for the end of output.
Shorter values mean quicker response, but is more CPU intensive."
  :type 'number
  :group 'imap)

(defcustom imap-store-password nil
  "If non-nil, store session password without prompting."
  :group 'imap
  :type 'boolean)

;; Various variables.

(defvar imap-fetch-data-hook nil
  "Hooks called after receiving each FETCH response.")

(defvar imap-streams '(gssapi kerberos4 starttls tls ssl network shell)
  "Priority of streams to consider when opening connection to server.")

(defvar imap-stream-alist
  '((gssapi    imap-gssapi-stream-p    imap-gssapi-open)
    (kerberos4 imap-kerberos4-stream-p imap-kerberos4-open)
    (tls       imap-tls-p              imap-tls-open)
    (ssl       imap-ssl-p              imap-ssl-open)
    (network   imap-network-p          imap-network-open)
    (shell     imap-shell-p            imap-shell-open)
    (starttls  imap-starttls-p         imap-starttls-open))
  "Definition of network streams.

\(NAME CHECK OPEN)

NAME names the stream, CHECK is a function returning non-nil if the
server support the stream and OPEN is a function for opening the
stream.")

(defvar imap-authenticators '(gssapi
			      kerberos4
			      digest-md5
			      cram-md5
			      ;;sasl
			      login
			      anonymous)
  "Priority of authenticators to consider when authenticating to server.")

(defvar imap-authenticator-alist
  '((gssapi     imap-gssapi-auth-p    imap-gssapi-auth)
    (kerberos4  imap-kerberos4-auth-p imap-kerberos4-auth)
    (sasl	imap-sasl-auth-p      imap-sasl-auth)
    (cram-md5   imap-cram-md5-p       imap-cram-md5-auth)
    (login      imap-login-p          imap-login-auth)
    (anonymous  imap-anonymous-p      imap-anonymous-auth)
    (digest-md5 imap-digest-md5-p     imap-digest-md5-auth))
  "Definition of authenticators.

\(NAME CHECK AUTHENTICATE)

NAME names the authenticator.  CHECK is a function returning non-nil if
the server support the authenticator and AUTHENTICATE is a function
for doing the actual authentication.")

(defvar imap-error nil
  "Error codes from the last command.")

(defvar imap-logout-timeout nil
  "Close server immediately if it can't logout in this number of seconds.
If it is nil, never close server until logout completes.  Normally,
the value of this variable will be bound to a certain value to which
an application program that uses this module specifies on a per-server
basis.")

;; Internal constants.  Change these and die.

(defconst imap-default-port 143)
(defconst imap-default-ssl-port 993)
(defconst imap-default-tls-port 993)
(defconst imap-default-stream 'network)
(defconst imap-coding-system-for-read 'binary)
(defconst imap-coding-system-for-write 'binary)
(defconst imap-local-variables '(imap-server
				 imap-port
				 imap-client-eol
				 imap-server-eol
				 imap-auth
				 imap-stream
				 imap-username
				 imap-password
				 imap-current-mailbox
				 imap-current-target-mailbox
				 imap-message-data
				 imap-capability
				 imap-id
				 imap-namespace
				 imap-state
				 imap-reached-tag
				 imap-failed-tags
				 imap-tag
				 imap-process
				 imap-calculate-literal-size-first
				 imap-mailbox-data))
(defconst imap-log-buffer "*imap-log*")
(defconst imap-debug-buffer "*imap-debug*")

;; Internal variables.

(defvar imap-stream nil)
(defvar imap-auth nil)
(defvar imap-server nil)
(defvar imap-port nil)
(defvar imap-username nil)
(defvar imap-password nil)
(defvar imap-last-authenticator nil)
(defvar imap-calculate-literal-size-first nil)
(defvar imap-state 'closed
  "IMAP state.
Valid states are `closed', `initial', `nonauth', `auth', `selected'
and `examine'.")

(defvar imap-server-eol "\r\n"
  "The EOL string sent from the server.")

(defvar imap-client-eol "\r\n"
  "The EOL string we send to the server.")

(defvar imap-current-mailbox nil
  "Current mailbox name.")

(defvar imap-current-target-mailbox nil
  "Current target mailbox for COPY and APPEND commands.")

(defvar imap-mailbox-data nil
  "Obarray with mailbox data.")

(defvar imap-mailbox-prime 997
  "Length of `imap-mailbox-data'.")

(defvar imap-current-message nil
  "Current message number.")

(defvar imap-message-data nil
  "Obarray with message data.")

(defvar imap-message-prime 997
  "Length of `imap-message-data'.")

(defvar imap-capability nil
  "Capability for server.")

(defvar imap-id nil
  "Identity of server.
See RFC 2971.")

(defvar imap-namespace nil
  "Namespace for current server.")

(defvar imap-reached-tag 0
  "Lower limit on command tags that have been parsed.")

(defvar imap-failed-tags nil
  "Alist of tags that failed.
Each element is a list with four elements; tag (a integer), response
state (a symbol, `OK', `NO' or `BAD'), response code (a string), and
human readable response text (a string).")

(defvar imap-tag 0
  "Command tag number.")

(defvar imap-process nil
  "Process.")

(defvar imap-continuation nil
  "Non-nil indicates that the server emitted a continuation request.
The actual value is really the text on the continuation line.")

(defvar imap-callbacks nil
  "List of response tags and callbacks, on the form `(number . function)'.
The function should take two arguments, the first the IMAP tag and the
second the status (OK, NO, BAD etc) of the command.")

(defvar imap-enable-exchange-bug-workaround nil
  "Send FETCH UID commands as *:* instead of *.

When non-nil, use an alternative UIDS form.  Enabling appears to
be required for some servers (e.g., Microsoft Exchange 2007)
which otherwise would trigger a response 'BAD The specified
message set is invalid.'.  We don't unconditionally use this
form, since this is said to be significantly inefficient.

This variable is set to t automatically per server if the
canonical form fails.")


;; Utility functions:

(defun imap-remassoc (key alist)
  "Delete by side effect any elements of ALIST whose car is `equal' to KEY.
The modified ALIST is returned.  If the first member
of ALIST has a car that is `equal' to KEY, there is no way to remove it
by side effect; therefore, write `(setq foo (remassoc key foo))' to be
sure of changing the value of `foo'."
  (when alist
    (if (equal key (caar alist))
	(cdr alist)
      (setcdr alist (imap-remassoc key (cdr alist)))
      alist)))

(defmacro imap-disable-multibyte ()
  "Enable multibyte in the current buffer."
  (unless (featurep 'xemacs)
    '(set-buffer-multibyte nil)))

(defsubst imap-utf7-encode (string)
  (if imap-use-utf7
      (and string
	   (condition-case ()
	       (utf7-encode string t)
	     (error (message
		     "imap: Could not UTF7 encode `%s', using it unencoded..."
		     string)
		    string)))
    string))

(defsubst imap-utf7-decode (string)
  (if imap-use-utf7
      (and string
	   (condition-case ()
	       (utf7-decode string t)
	     (error (message
		     "imap: Could not UTF7 decode `%s', using it undecoded..."
		     string)
		    string)))
    string))

(defsubst imap-ok-p (status)
  (if (eq status 'OK)
      t
    (setq imap-error status)
    nil))

(defun imap-error-text (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (nth 3 (car imap-failed-tags))))


;; Server functions; stream stuff:

(defun imap-log (string-or-buffer)
  (when imap-log
    (with-current-buffer (get-buffer-create imap-log-buffer)
      (imap-disable-multibyte)
      (buffer-disable-undo)
      (goto-char (point-max))
      (if (bufferp string-or-buffer)
	  (insert-buffer-substring string-or-buffer)
	(insert string-or-buffer)))))

(defun imap-kerberos4-stream-p (buffer)
  (imap-capability 'AUTH=KERBEROS_V4 buffer))

(defun imap-kerberos4-open (name buffer server port)
  (let ((cmds imap-kerberos4-program)
	cmd done)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "Opening Kerberos 4 IMAP connection with `%s'..." cmd)
      (erase-buffer)
      (let* ((port (or port imap-default-port))
	     (coding-system-for-read imap-coding-system-for-read)
	     (coding-system-for-write imap-coding-system-for-write)
	     (process-connection-type imap-process-connection-type)
	     (process (start-process
		       name buffer shell-file-name shell-command-switch
		       (format-spec
			cmd
			(format-spec-make
			 ?s server
			 ?p (number-to-string port)
			 ?l imap-default-user))))
	     response)
	(when process
	  (with-current-buffer buffer
	    (setq imap-client-eol "\n"
		  imap-calculate-literal-size-first t)
	    (while (and (memq (process-status process) '(open run))
			(set-buffer buffer) ;; XXX "blue moon" nntp.el bug
			(goto-char (point-min))
			;; Athena IMTEST can output SSL verify errors
			(or (while (looking-at "^verify error:num=")
			      (forward-line))
			    t)
			(or (while (looking-at "^TLS connection established")
			      (forward-line))
			    t)
			;; cyrus 1.6.x (13? < x <= 22) queries capabilities
			(or (while (looking-at "^C:")
			      (forward-line))
			    t)
			;; cyrus 1.6 imtest print "S: " before server greeting
			(or (not (looking-at "S: "))
			    (forward-char 3)
			    t)
			(not (and (imap-parse-greeting)
				  ;; success in imtest < 1.6:
				  (or (re-search-forward
				       "^__\\(.*\\)__\n" nil t)
				      ;; success in imtest 1.6:
				      (re-search-forward
				       "^\\(Authenticat.*\\)" nil t))
				  (setq response (match-string 1)))))
	      (accept-process-output process 1)
	      (sit-for 1))
	    (erase-buffer)
	    (message "Opening Kerberos 4 IMAP connection with `%s'...%s" cmd
		     (if response (concat "done, " response) "failed"))
	    (if (and response (let ((case-fold-search nil))
				(not (string-match "failed" response))))
		(setq done process)
	      (if (memq (process-status process) '(open run))
		  (imap-logout))
	      (delete-process process)
	      nil)))))
    done))

(defun imap-gssapi-stream-p (buffer)
  (imap-capability 'AUTH=GSSAPI buffer))

(defun imap-gssapi-open (name buffer server port)
  (let ((cmds imap-gssapi-program)
	cmd done)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "Opening GSSAPI IMAP connection with `%s'..." cmd)
      (erase-buffer)
      (let* ((port (or port imap-default-port))
	     (coding-system-for-read imap-coding-system-for-read)
	     (coding-system-for-write imap-coding-system-for-write)
	     (process-connection-type imap-process-connection-type)
	     (process (start-process
		       name buffer shell-file-name shell-command-switch
		       (format-spec
			cmd
			(format-spec-make
			 ?s server
			 ?p (number-to-string port)
			 ?l imap-default-user))))
	     response)
	(when process
	  (with-current-buffer buffer
	    (setq imap-client-eol "\n"
		  imap-calculate-literal-size-first t)
	    (while (and (memq (process-status process) '(open run))
			(set-buffer buffer) ;; XXX "blue moon" nntp.el bug
			(goto-char (point-min))
			;; Athena IMTEST can output SSL verify errors
			(or (while (looking-at "^verify error:num=")
			      (forward-line))
			    t)
			(or (while (looking-at "^TLS connection established")
			      (forward-line))
			    t)
			;; cyrus 1.6.x (13? < x <= 22) queries capabilities
			(or (while (looking-at "^C:")
			      (forward-line))
			    t)
			;; cyrus 1.6 imtest print "S: " before server greeting
			(or (not (looking-at "S: "))
			    (forward-char 3)
			    t)
			;; GNU SASL may print 'Trying ...' first.
			(or (not (looking-at "Trying "))
			    (forward-line)
			    t)
			(not (and (imap-parse-greeting)
				  ;; success in imtest 1.6:
				  (re-search-forward
				   (concat "^\\(\\(Authenticat.*\\)\\|\\("
					   "Client authentication "
					   "finished.*\\)\\)")
				   nil t)
				  (setq response (match-string 1)))))
	      (accept-process-output process 1)
	      (sit-for 1))
	    (imap-log buffer)
	    (erase-buffer)
	    (message "GSSAPI IMAP connection: %s" (or response "failed"))
	    (if (and response (let ((case-fold-search nil))
				(not (string-match "failed" response))))
		(setq done process)
	      (if (memq (process-status process) '(open run))
		  (imap-logout))
	      (delete-process process)
	      nil)))))
    done))

(defun imap-ssl-p (buffer)
  nil)

(defun imap-ssl-open (name buffer server port)
  "Open an SSL connection to SERVER."
  (let ((cmds (if (listp imap-ssl-program) imap-ssl-program
		(list imap-ssl-program)))
	cmd done)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "imap: Opening SSL connection with `%s'..." cmd)
      (erase-buffer)
      (let* ((port (or port imap-default-ssl-port))
	     (coding-system-for-read imap-coding-system-for-read)
	     (coding-system-for-write imap-coding-system-for-write)
	     (process-connection-type imap-process-connection-type)
	     (set-process-query-on-exit-flag
	      (if (fboundp 'set-process-query-on-exit-flag)
		  'set-process-query-on-exit-flag
		'process-kill-without-query))
	     process)
	(when (progn
		(setq process (start-process
			       name buffer shell-file-name
			       shell-command-switch
			       (format-spec cmd
					    (format-spec-make
					     ?s server
					     ?p (number-to-string port)))))
		(funcall set-process-query-on-exit-flag process nil)
		process)
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (while (and (memq (process-status process) '(open run))
			(set-buffer buffer) ;; XXX "blue moon" nntp.el bug
			(goto-char (point-max))
			(forward-line -1)
			(not (imap-parse-greeting)))
	      (accept-process-output process 1)
	      (sit-for 1))
	    (imap-log buffer)
	    (erase-buffer)
	    (when (memq (process-status process) '(open run))
	      (setq done process))))))
    (if done
	(progn
	  (message "imap: Opening SSL connection with `%s'...done" cmd)
	  done)
      (message "imap: Opening SSL connection with `%s'...failed" cmd)
      nil)))

(defun imap-tls-p (buffer)
  nil)

(defun imap-tls-open (name buffer server port)
  (let* ((port (or port imap-default-tls-port))
	 (coding-system-for-read imap-coding-system-for-read)
	 (coding-system-for-write imap-coding-system-for-write)
	 (process (open-tls-stream name buffer server port)))
    (when process
      (while (and (memq (process-status process) '(open run))
		  ;; FIXME: Per the "blue moon" comment, the process/buffer
		  ;; handling here, and elsewhere in functions which open
		  ;; streams, looks confused.  Obviously we can change buffers
		  ;; if a different process handler kicks in from
		  ;; `accept-process-output' or `sit-for' below, and TRT seems
		  ;; to be to `save-buffer' around those calls.  (I wonder why
		  ;; `sit-for' is used with a non-zero wait.)  -- fx
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-max))
		  (forward-line -1)
		  (not (imap-parse-greeting)))
	(accept-process-output process 1)
	(sit-for 1))
      (imap-log buffer)
      (when (memq (process-status process) '(open run))
	process))))

(defun imap-network-p (buffer)
  t)

(defun imap-network-open (name buffer server port)
  (let* ((port (or port imap-default-port))
	 (coding-system-for-read imap-coding-system-for-read)
	 (coding-system-for-write imap-coding-system-for-write)
	 (process (open-network-stream name buffer server port)))
    (when process
      (while (and (memq (process-status process) '(open run))
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-min))
		  (not (imap-parse-greeting)))
	(accept-process-output process 1)
	(sit-for 1))
      (imap-log buffer)
      (when (memq (process-status process) '(open run))
	process))))

(defun imap-shell-p (buffer)
  nil)

(defun imap-shell-open (name buffer server port)
  (let ((cmds (if (listp imap-shell-program) imap-shell-program
		(list imap-shell-program)))
	cmd done)
    (while (and (not done) (setq cmd (pop cmds)))
      (message "imap: Opening IMAP connection with `%s'..." cmd)
      (setq imap-client-eol "\n")
      (let* ((port (or port imap-default-port))
	     (coding-system-for-read imap-coding-system-for-read)
	     (coding-system-for-write imap-coding-system-for-write)
             (process-connection-type imap-process-connection-type)
	     (process (start-process
		       name buffer shell-file-name shell-command-switch
		       (format-spec
			cmd
			(format-spec-make
			 ?s server
			 ?g imap-shell-host
			 ?p (number-to-string port)
			 ?l imap-default-user)))))
	(when process
	  (while (and (memq (process-status process) '(open run))
		      (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		      (goto-char (point-max))
		      (forward-line -1)
		      (not (imap-parse-greeting)))
	    (accept-process-output process 1)
	    (sit-for 1))
	  (imap-log buffer)
	  (erase-buffer)
	  (when (memq (process-status process) '(open run))
	    (setq done process)))))
    (if done
	(progn
	  (message "imap: Opening IMAP connection with `%s'...done" cmd)
	  done)
      (message "imap: Opening IMAP connection with `%s'...failed" cmd)
      nil)))

(defun imap-starttls-p (buffer)
  (imap-capability 'STARTTLS buffer))

(defun imap-starttls-open (name buffer server port)
  (let* ((port (or port imap-default-port))
	 (coding-system-for-read imap-coding-system-for-read)
	 (coding-system-for-write imap-coding-system-for-write)
	 (process (starttls-open-stream name buffer server port))
	 done tls-info)
    (message "imap: Connecting with STARTTLS...")
    (when process
      (while (and (memq (process-status process) '(open run))
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-max))
		  (forward-line -1)
		  (not (imap-parse-greeting)))
	(accept-process-output process 1)
	(sit-for 1))
      (imap-send-command "STARTTLS")
      (while (and (memq (process-status process) '(open run))
		  (set-buffer buffer) ;; XXX "blue moon" nntp.el bug
		  (goto-char (point-max))
		  (forward-line -1)
		  (not (re-search-forward "[0-9]+ OK.*\r?\n" nil t)))
	(accept-process-output process 1)
	(sit-for 1))
      (imap-log buffer)
      (when (and (setq tls-info (starttls-negotiate process))
		 (memq (process-status process) '(open run)))
	(setq done process)))
    (if (stringp tls-info)
	(message "imap: STARTTLS info: %s" tls-info))
    (message "imap: Connecting with STARTTLS...%s" (if done "done" "failed"))
    done))

;; Server functions; authenticator stuff:

(defun imap-interactive-login (buffer loginfunc)
  "Login to server in BUFFER.
LOGINFUNC is passed a username and a password, it should return t if
it where successful authenticating itself to the server, nil otherwise.
Returns t if login was successful, nil otherwise."
  (with-current-buffer buffer
    (make-local-variable 'imap-username)
    (make-local-variable 'imap-password)
    (let (user passwd ret)
      ;;      (condition-case ()
      (while (or (not user) (not passwd))
	(setq user (or imap-username
		       (read-from-minibuffer
			(concat "imap: username for " imap-server
				" (using stream `" (symbol-name imap-stream)
				"'): ")
			(or user imap-default-user))))
	(setq passwd (or imap-password
			 (read-passwd
			  (concat "imap: password for " user "@"
				  imap-server " (using authenticator `"
				  (symbol-name imap-auth) "'): "))))
	(when (and user passwd)
	  (if (funcall loginfunc user passwd)
	      (progn
		(message "imap: Login successful...")
		(setq ret t
		      imap-username user)
		(when (and (not imap-password)
			   (or imap-store-password
			       (y-or-n-p "imap: Store password for this IMAP session? ")))
		  (setq imap-password passwd)))
	    (message "imap: Login failed...")
	    (setq passwd nil)
	    (setq imap-password nil)
	    (sit-for 1))))
      ;;	(quit (with-current-buffer buffer
      ;;		(setq user nil
      ;;		      passwd nil)))
      ;;	(error (with-current-buffer buffer
      ;;		 (setq user nil
      ;;		       passwd nil))))
      ret)))

(defun imap-gssapi-auth-p (buffer)
  (eq imap-stream 'gssapi))

(defun imap-gssapi-auth (buffer)
  (message "imap: Authenticating using GSSAPI...%s"
	   (if (eq imap-stream 'gssapi) "done" "failed"))
  (eq imap-stream 'gssapi))

(defun imap-kerberos4-auth-p (buffer)
  (and (imap-capability 'AUTH=KERBEROS_V4 buffer)
       (eq imap-stream 'kerberos4)))

(defun imap-kerberos4-auth (buffer)
  (message "imap: Authenticating using Kerberos 4...%s"
	   (if (eq imap-stream 'kerberos4) "done" "failed"))
  (eq imap-stream 'kerberos4))

(defun imap-cram-md5-p (buffer)
  (imap-capability 'AUTH=CRAM-MD5 buffer))

(defun imap-cram-md5-auth (buffer)
  "Login to server using the AUTH CRAM-MD5 method."
  (message "imap: Authenticating using CRAM-MD5...")
  (let ((done (imap-interactive-login
	       buffer
	       (lambda (user passwd)
		 (imap-ok-p
		  (imap-send-command-wait
		   (list
		    "AUTHENTICATE CRAM-MD5"
		    (lambda (challenge)
		      (let* ((decoded (base64-decode-string challenge))
			     (hash (rfc2104-hash 'md5 64 16 passwd decoded))
			     (response (concat user " " hash))
			     (encoded (base64-encode-string response)))
			encoded)))))))))
    (if done
	(message "imap: Authenticating using CRAM-MD5...done")
      (message "imap: Authenticating using CRAM-MD5...failed"))))

(defun imap-login-p (buffer)
  (and (not (imap-capability 'LOGINDISABLED buffer))
       (not (imap-capability 'X-LOGIN-CMD-DISABLED buffer))))

(defun imap-quote-specials (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "[\\\"]" nil t)
      (forward-char -1)
      (insert "\\")
      (forward-char 1))
    (buffer-string)))

(defun imap-login-auth (buffer)
  "Login to server using the LOGIN command."
  (message "imap: Plaintext authentication...")
  (imap-interactive-login buffer
			  (lambda (user passwd)
			    (imap-ok-p (imap-send-command-wait
					(concat "LOGIN \""
						(imap-quote-specials user)
						"\" \""
						(imap-quote-specials passwd)
						"\""))))))

(defun imap-anonymous-p (buffer)
  t)

(defun imap-anonymous-auth (buffer)
  (message "imap: Logging in anonymously...")
  (with-current-buffer buffer
    (imap-ok-p (imap-send-command-wait
		(concat "LOGIN anonymous \"" (concat (user-login-name) "@"
						     (system-name)) "\"")))))

;;; Compiler directives.

(defvar imap-sasl-client)
(defvar imap-sasl-step)

(defun imap-sasl-make-mechanisms (buffer)
  (let ((mecs '()))
    (mapc (lambda (sym)
	    (let ((name (symbol-name sym)))
	      (if (and (> (length name) 5)
		       (string-equal "AUTH=" (substring name 0 5 )))
		  (setq mecs (cons (substring name 5) mecs)))))
	  (imap-capability nil buffer))
    mecs))

(declare-function sasl-find-mechanism "sasl" (mechanism))
(declare-function sasl-mechanism-name "sasl" (mechanism))
(declare-function sasl-make-client    "sasl" (mechanism name service server))
(declare-function sasl-next-step      "sasl" (client step))
(declare-function sasl-step-data      "sasl" (step))
(declare-function sasl-step-set-data  "sasl" (step data))

(defun imap-sasl-auth-p (buffer)
  (and (condition-case ()
	   (require 'sasl)
	 (error nil))
       (sasl-find-mechanism (imap-sasl-make-mechanisms buffer))))

(defun imap-sasl-auth (buffer)
  "Login to server using the SASL method."
  (message "imap: Authenticating using SASL...")
  (with-current-buffer buffer
    (make-local-variable 'imap-username)
    (make-local-variable 'imap-sasl-client)
    (make-local-variable 'imap-sasl-step)
    (let ((mechanism (sasl-find-mechanism (imap-sasl-make-mechanisms buffer)))
	  logged user)
      (while (not logged)
	(setq user (or imap-username
		       (read-from-minibuffer
			(concat "IMAP username for " imap-server " using SASL "
				(sasl-mechanism-name mechanism) ": ")
			(or user imap-default-user))))
	(when user
	  (setq imap-sasl-client (sasl-make-client mechanism user "imap2" imap-server)
		imap-sasl-step (sasl-next-step imap-sasl-client nil))
	  (let ((tag (imap-send-command
		      (if (sasl-step-data imap-sasl-step)
			  (format "AUTHENTICATE %s %s"
				  (sasl-mechanism-name mechanism)
				  (sasl-step-data imap-sasl-step))
			(format "AUTHENTICATE %s" (sasl-mechanism-name mechanism)))
		      buffer)))
	    (while (eq (imap-wait-for-tag tag) 'INCOMPLETE)
	      (sasl-step-set-data imap-sasl-step (base64-decode-string imap-continuation))
	      (setq imap-continuation nil
		    imap-sasl-step (sasl-next-step imap-sasl-client imap-sasl-step))
	      (imap-send-command-1 (if (sasl-step-data imap-sasl-step)
				       (base64-encode-string (sasl-step-data imap-sasl-step) t)
				     "")))
	    (if (imap-ok-p (imap-wait-for-tag tag))
		(setq imap-username user
		      logged t)
	      (message "Login failed...")
	      (sit-for 1)))))
      logged)))

(defun imap-digest-md5-p (buffer)
  (and (imap-capability 'AUTH=DIGEST-MD5 buffer)
       (condition-case ()
	   (require 'digest-md5)
	 (error nil))))

(defun imap-digest-md5-auth (buffer)
  "Login to server using the AUTH DIGEST-MD5 method."
  (message "imap: Authenticating using DIGEST-MD5...")
  (imap-interactive-login
   buffer
   (lambda (user passwd)
     (let ((tag
	    (imap-send-command
	     (list
	      "AUTHENTICATE DIGEST-MD5"
	      (lambda (challenge)
		(digest-md5-parse-digest-challenge
		 (base64-decode-string challenge))
		(let* ((digest-uri
			(digest-md5-digest-uri
			 "imap" (digest-md5-challenge 'realm)))
		       (response
			(digest-md5-digest-response
			 user passwd digest-uri)))
		  (base64-encode-string response 'no-line-break))))
	     )))
       (if (not (eq (imap-wait-for-tag tag) 'INCOMPLETE))
	   nil
	 (setq imap-continuation nil)
	 (imap-send-command-1 "")
	 (imap-ok-p (imap-wait-for-tag tag)))))))

;; Server functions:

(defun imap-open-1 (buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (setq imap-current-mailbox nil
	  imap-current-message nil
	  imap-state 'initial
	  imap-process (condition-case ()
			   (funcall (nth 2 (assq imap-stream
						 imap-stream-alist))
				    "imap" buffer imap-server imap-port)
			 ((error quit) nil)))
    (when imap-process
      (set-process-filter imap-process 'imap-arrival-filter)
      (set-process-sentinel imap-process 'imap-sentinel)
      (while (and (eq imap-state 'initial)
		  (memq (process-status imap-process) '(open run)))
	(message "Waiting for response from %s..." imap-server)
	(accept-process-output imap-process 1))
      (message "Waiting for response from %s...done" imap-server)
      (and (memq (process-status imap-process) '(open run))
	   imap-process))))

(defun imap-open (server &optional port stream auth buffer)
  "Open an IMAP connection to host SERVER at PORT returning a buffer.
If PORT is unspecified, a default value is used (143 except
for SSL which use 993).
STREAM indicates the stream to use, see `imap-streams' for available
streams.  If nil, it choices the best stream the server is capable of.
AUTH indicates authenticator to use, see `imap-authenticators' for
available authenticators.  If nil, it choices the best stream the
server is capable of.
BUFFER can be a buffer or a name of a buffer, which is created if
necessary.  If nil, the buffer name is generated."
  (setq buffer (or buffer (format " *imap* %s:%d" server (or port 0))))
  (with-current-buffer (get-buffer-create buffer)
    (if (imap-opened buffer)
	(imap-close buffer))
    (mapc 'make-local-variable imap-local-variables)
    (imap-disable-multibyte)
    (buffer-disable-undo)
    (setq imap-server (or server imap-server))
    (setq imap-port (or port imap-port))
    (setq imap-auth (or auth imap-auth))
    (setq imap-stream (or stream imap-stream))
    (message "imap: Connecting to %s..." imap-server)
    (if (null (let ((imap-stream (or imap-stream imap-default-stream)))
		(imap-open-1 buffer)))
	(progn
	  (message "imap: Connecting to %s...failed" imap-server)
	  nil)
      (when (null imap-stream)
	;; Need to choose stream.
	(let ((streams imap-streams))
	  (while (setq stream (pop streams))
	    ;; OK to use this stream?
	    (when (funcall (nth 1 (assq stream imap-stream-alist)) buffer)
	      ;; Stream changed?
	      (if (not (eq imap-default-stream stream))
		  (with-current-buffer (get-buffer-create
					(generate-new-buffer-name " *temp*"))
		    (mapc 'make-local-variable imap-local-variables)
		    (imap-disable-multibyte)
		    (buffer-disable-undo)
		    (setq imap-server (or server imap-server))
		    (setq imap-port (or port imap-port))
		    (setq imap-auth (or auth imap-auth))
		    (message "imap: Reconnecting with stream `%s'..." stream)
		    (if (null (let ((imap-stream stream))
				(imap-open-1 (current-buffer))))
			(progn
			  (kill-buffer (current-buffer))
			  (message
			   "imap: Reconnecting with stream `%s'...failed"
			   stream))
		      ;; We're done, kill the first connection
		      (imap-close buffer)
		      (let ((name (if (stringp buffer)
				      buffer
				    (buffer-name buffer))))
			(kill-buffer buffer)
			(rename-buffer name)
			;; set the passed buffer to the current one,
			;; so that (imap-opened buffer) later will work
			(setq buffer (current-buffer)))
		      (message "imap: Reconnecting with stream `%s'...done"
			       stream)
		      (setq imap-stream stream)
		      (setq imap-capability nil)
		      (setq streams nil)))
		;; We're done
		(message "imap: Connecting to %s...done" imap-server)
		(setq imap-stream stream)
		(setq imap-capability nil)
		(setq streams nil))))))
      (when (imap-opened buffer)
	(setq imap-mailbox-data (make-vector imap-mailbox-prime 0)))
      ;; (debug "opened+state+auth+buffer" (imap-opened buffer) imap-state imap-auth buffer)
      (when imap-stream
	buffer))))

(defcustom imap-ping-server t
  "If non-nil, check if IMAP is open.
See the function `imap-ping-server'."
  :version "23.1" ;; No Gnus
  :group 'imap
  :type 'boolean)

(defun imap-opened (&optional buffer)
  "Return non-nil if connection to imap server in BUFFER is open.
If BUFFER is nil then the current buffer is used."
  (and (setq buffer (get-buffer (or buffer (current-buffer))))
       (buffer-live-p buffer)
       (with-current-buffer buffer
	 (and imap-process
	      (memq (process-status imap-process) '(open run))
	      (if imap-ping-server
		  (imap-ping-server)
		t)))))

(defun imap-ping-server (&optional buffer)
  "Ping the IMAP server in BUFFER with a \"NOOP\" command.
Return non-nil if the server responds, and nil if it does not
respond.  If BUFFER is nil, the current buffer is used."
  (condition-case ()
      (imap-ok-p (imap-send-command-wait "NOOP" buffer))
    (error nil)))

(defun imap-authenticate (&optional user passwd buffer)
  "Authenticate to server in BUFFER, using current buffer if nil.
It uses the authenticator specified when opening the server.  If the
authenticator requires username/passwords, they are queried from the
user and optionally stored in the buffer.  If USER and/or PASSWD is
specified, the user will not be questioned and the username and/or
password is remembered in the buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if (not (eq imap-state 'nonauth))
	(or (eq imap-state 'auth)
	    (eq imap-state 'selected)
	    (eq imap-state 'examine))
      (make-local-variable 'imap-username)
      (make-local-variable 'imap-password)
      (make-local-variable 'imap-last-authenticator)
      (when user (setq imap-username user))
      (when passwd (setq imap-password passwd))
      (if imap-auth
	  (and (setq imap-last-authenticator
		     (assq imap-auth imap-authenticator-alist))
	       (funcall (nth 2 imap-last-authenticator) (current-buffer))
	       (setq imap-state 'auth))
	;; Choose authenticator.
	(let ((auths imap-authenticators)
	      auth)
	  (while (setq auth (pop auths))
	    ;; OK to use authenticator?
	    (setq imap-last-authenticator
		  (assq auth imap-authenticator-alist))
	    (when (funcall (nth 1 imap-last-authenticator) (current-buffer))
	      (message "imap: Authenticating to `%s' using `%s'..."
		       imap-server auth)
	      (setq imap-auth auth)
	      (if (funcall (nth 2 imap-last-authenticator) (current-buffer))
		  (progn
		    (message "imap: Authenticating to `%s' using `%s'...done"
			     imap-server auth)
		    ;; set imap-state correctly on successful auth attempt
		    (setq imap-state 'auth)
		    ;; stop iterating through the authenticator list
		    (setq auths nil))
		(message "imap: Authenticating to `%s' using `%s'...failed"
			 imap-server auth)))))
	imap-state))))

(defun imap-close (&optional buffer)
  "Close connection to server in BUFFER.
If BUFFER is nil, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (when (imap-opened)
      (condition-case nil
	  (imap-logout-wait)
	(quit nil)))
    (when (and imap-process
	       (memq (process-status imap-process) '(open run)))
      (delete-process imap-process))
    (setq imap-current-mailbox nil
	  imap-current-message nil
	  imap-process nil)
    (erase-buffer)
    t))

(defun imap-capability (&optional identifier buffer)
  "Return a list of identifiers which server in BUFFER support.
If IDENTIFIER, return non-nil if it's among the servers capabilities.
If BUFFER is nil, the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (unless imap-capability
      (unless (imap-ok-p (imap-send-command-wait "CAPABILITY"))
	(setq imap-capability '(IMAP2))))
    (if identifier
	(memq (intern (upcase (symbol-name identifier))) imap-capability)
      imap-capability)))

(defun imap-id (&optional list-of-values buffer)
  "Identify client to server in BUFFER, and return server identity.
LIST-OF-VALUES is nil, or a plist with identifier and value
strings to send to the server to identify the client.

Return a list of identifiers which server in BUFFER support, or
nil if it doesn't support ID or returns no information.

If BUFFER is nil, the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (when (and (imap-capability 'ID)
	       (imap-ok-p (imap-send-command-wait
			   (if (null list-of-values)
			       "ID NIL"
			     (concat "ID (" (mapconcat (lambda (el)
							 (concat "\"" el "\""))
						       list-of-values
						       " ") ")")))))
      imap-id)))

(defun imap-namespace (&optional buffer)
  "Return a namespace hierarchy at server in BUFFER.
If BUFFER is nil, the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (unless imap-namespace
      (when (imap-capability 'NAMESPACE)
	(imap-send-command-wait "NAMESPACE")))
    imap-namespace))

(defun imap-send-command-wait (command &optional buffer)
  (imap-wait-for-tag (imap-send-command command buffer) buffer))

(defun imap-logout (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (if imap-logout-timeout
      (with-timeout (imap-logout-timeout
		     (condition-case nil
			 (with-current-buffer buffer
			   (delete-process imap-process))
		       (error)))
	(imap-send-command "LOGOUT" buffer))
    (imap-send-command "LOGOUT" buffer)))

(defun imap-logout-wait (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (if imap-logout-timeout
      (with-timeout (imap-logout-timeout
		     (condition-case nil
			 (with-current-buffer buffer
			   (delete-process imap-process))
		       (error)))
	(imap-send-command-wait "LOGOUT" buffer))
    (imap-send-command-wait "LOGOUT" buffer)))


;; Mailbox functions:

(defun imap-mailbox-put (propname value &optional mailbox buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if imap-mailbox-data
	(put (intern (or mailbox imap-current-mailbox) imap-mailbox-data)
	     propname value)
      (error "Imap-mailbox-data is nil, prop %s value %s mailbox %s buffer %s"
	     propname value mailbox (current-buffer)))
    t))

(defsubst imap-mailbox-get-1 (propname &optional mailbox)
  (get (intern-soft (or mailbox imap-current-mailbox) imap-mailbox-data)
       propname))

(defun imap-mailbox-get (propname &optional mailbox buffer)
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-mailbox-get-1 propname (or mailbox imap-current-mailbox)))))

(defun imap-mailbox-map-1 (func &optional mailbox-decoder buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let (result)
      (mapatoms
       (lambda (s)
	 (push (funcall func (if mailbox-decoder
				 (funcall mailbox-decoder (symbol-name s))
			       (symbol-name s))) result))
       imap-mailbox-data)
      result)))

(defun imap-mailbox-map (func &optional buffer)
  "Map a function across each mailbox in `imap-mailbox-data', returning a list.
Function should take a mailbox name (a string) as
the only argument."
  (imap-mailbox-map-1 func 'imap-utf7-decode buffer))

(defun imap-current-mailbox (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-utf7-decode imap-current-mailbox)))

(defun imap-current-mailbox-p-1 (mailbox &optional examine)
  (and (string= mailbox imap-current-mailbox)
       (or (and examine
		(eq imap-state 'examine))
	   (and (not examine)
		(eq imap-state 'selected)))))

(defun imap-current-mailbox-p (mailbox &optional examine buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-current-mailbox-p-1 (imap-utf7-encode mailbox) examine)))

(defun imap-mailbox-select-1 (mailbox &optional examine)
  "Select MAILBOX on server in BUFFER.
If EXAMINE is non-nil, do a read-only select."
  (if (imap-current-mailbox-p-1 mailbox examine)
      imap-current-mailbox
    (setq imap-current-mailbox mailbox)
    (if (imap-ok-p (imap-send-command-wait
		    (concat (if examine "EXAMINE" "SELECT") " \""
			    mailbox "\"")))
	(progn
	  (setq imap-message-data (make-vector imap-message-prime 0)
		imap-state (if examine 'examine 'selected))
	  imap-current-mailbox)
      ;; Failed SELECT/EXAMINE unselects current mailbox
      (setq imap-current-mailbox nil))))

(defun imap-mailbox-select (mailbox &optional examine buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-utf7-decode
     (imap-mailbox-select-1 (imap-utf7-encode mailbox) examine))))

(defun imap-mailbox-examine-1 (mailbox &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-mailbox-select-1 mailbox 'examine)))

(defun imap-mailbox-examine (mailbox &optional buffer)
  "Examine MAILBOX on server in BUFFER."
  (imap-mailbox-select mailbox 'examine buffer))

(defun imap-mailbox-unselect (&optional buffer)
  "Close current folder in BUFFER, without expunging articles."
  (with-current-buffer (or buffer (current-buffer))
    (when (or (eq imap-state 'auth)
	      (and (imap-capability 'UNSELECT)
		   (imap-ok-p (imap-send-command-wait "UNSELECT")))
	      (and (imap-ok-p
		    (imap-send-command-wait (concat "EXAMINE \""
						    imap-current-mailbox
						    "\"")))
		   (imap-ok-p (imap-send-command-wait "CLOSE"))))
      (setq imap-current-mailbox nil
	    imap-message-data nil
	    imap-state 'auth)
      t)))

(defun imap-mailbox-expunge (&optional asynch buffer)
  "Expunge articles in current folder in BUFFER.
If ASYNCH, do not wait for successful completion of the command.
If BUFFER is nil the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (when (and imap-current-mailbox (not (eq imap-state 'examine)))
      (if asynch
	  (imap-send-command "EXPUNGE")
      (imap-ok-p (imap-send-command-wait "EXPUNGE"))))))

(defun imap-mailbox-close (&optional asynch buffer)
  "Expunge articles and close current folder in BUFFER.
If ASYNCH, do not wait for successful completion of the command.
If BUFFER is nil the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (when imap-current-mailbox
      (if asynch
	  (imap-add-callback (imap-send-command "CLOSE")
			     `(lambda (tag status)
				(message "IMAP mailbox `%s' closed... %s"
					 imap-current-mailbox status)
				(when (eq ,imap-current-mailbox
					  imap-current-mailbox)
				  ;; Don't wipe out data if another mailbox
				  ;; was selected...
				  (setq imap-current-mailbox nil
					imap-message-data nil
					imap-state 'auth))))
	(when (imap-ok-p (imap-send-command-wait "CLOSE"))
	  (setq imap-current-mailbox nil
		imap-message-data nil
		imap-state 'auth)))
      t)))

(defun imap-mailbox-create-1 (mailbox)
  (imap-ok-p (imap-send-command-wait (list "CREATE \"" mailbox "\""))))

(defun imap-mailbox-create (mailbox &optional buffer)
  "Create MAILBOX on server in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (with-current-buffer (or buffer (current-buffer))
    (imap-mailbox-create-1 (imap-utf7-encode mailbox))))

(defun imap-mailbox-delete (mailbox &optional buffer)
  "Delete MAILBOX on server in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "DELETE \"" mailbox "\""))))))

(defun imap-mailbox-rename (oldname newname &optional buffer)
  "Rename mailbox OLDNAME to NEWNAME on server in BUFFER.
If BUFFER is nil the current buffer is assumed."
  (let ((oldname (imap-utf7-encode oldname))
	(newname (imap-utf7-encode newname)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "RENAME \"" oldname "\" "
				     "\"" newname "\""))))))

(defun imap-mailbox-lsub (&optional root reference add-delimiter buffer)
  "Return a list of subscribed mailboxes on server in BUFFER.
If ROOT is non-nil, only list matching mailboxes.  If ADD-DELIMITER is
non-nil, a hierarchy delimiter is added to root.  REFERENCE is a
implementation-specific string that has to be passed to lsub command."
  (with-current-buffer (or buffer (current-buffer))
    ;; Make sure we know the hierarchy separator for root's hierarchy
    (when (and add-delimiter (null (imap-mailbox-get-1 'delimiter root)))
      (imap-send-command-wait (concat "LIST \"" reference "\" \""
				      (imap-utf7-encode root) "\"")))
    ;; clear list data (NB not delimiter and other stuff)
    (imap-mailbox-map-1 (lambda (mailbox)
			  (imap-mailbox-put 'lsub nil mailbox)))
    (when (imap-ok-p
	   (imap-send-command-wait
	    (concat "LSUB \"" reference "\" \"" (imap-utf7-encode root)
		    (and add-delimiter (imap-mailbox-get-1 'delimiter root))
		    "%\"")))
      (let (out)
	(imap-mailbox-map-1 (lambda (mailbox)
			      (when (imap-mailbox-get-1 'lsub mailbox)
				(push (imap-utf7-decode mailbox) out))))
	(nreverse out)))))

(defun imap-mailbox-list (root &optional reference add-delimiter buffer)
  "Return a list of mailboxes matching ROOT on server in BUFFER.
If ADD-DELIMITER is non-nil, a hierarchy delimiter is added to
root.  REFERENCE is a implementation-specific string that has to be
passed to list command."
  (with-current-buffer (or buffer (current-buffer))
    ;; Make sure we know the hierarchy separator for root's hierarchy
    (when (and add-delimiter (null (imap-mailbox-get-1 'delimiter root)))
      (imap-send-command-wait (concat "LIST \"" reference "\" \""
				      (imap-utf7-encode root) "\"")))
    ;; clear list data (NB not delimiter and other stuff)
    (imap-mailbox-map-1 (lambda (mailbox)
			  (imap-mailbox-put 'list nil mailbox)))
    (when (imap-ok-p
	   (imap-send-command-wait
	    (concat "LIST \"" reference "\" \"" (imap-utf7-encode root)
		    (and add-delimiter (imap-mailbox-get-1 'delimiter root))
		    "%\"")))
      (let (out)
	(imap-mailbox-map-1 (lambda (mailbox)
			      (when (imap-mailbox-get-1 'list mailbox)
				(push (imap-utf7-decode mailbox) out))))
	(nreverse out)))))

(defun imap-mailbox-subscribe (mailbox &optional buffer)
  "Send the SUBSCRIBE command on the MAILBOX to server in BUFFER.
Returns non-nil if successful."
  (with-current-buffer (or buffer (current-buffer))
    (imap-ok-p (imap-send-command-wait (concat "SUBSCRIBE \""
					       (imap-utf7-encode mailbox)
					       "\"")))))

(defun imap-mailbox-unsubscribe (mailbox &optional buffer)
  "Send the SUBSCRIBE command on the MAILBOX to server in BUFFER.
Returns non-nil if successful."
  (with-current-buffer (or buffer (current-buffer))
    (imap-ok-p (imap-send-command-wait (concat "UNSUBSCRIBE "
					       (imap-utf7-encode mailbox)
					       "\"")))))

(defun imap-mailbox-status (mailbox items &optional buffer)
  "Get status items ITEM in MAILBOX from server in BUFFER.
ITEMS can be a symbol or a list of symbols, valid symbols are one of
the STATUS data items -- i.e. `messages', `recent', `uidnext', `uidvalidity',
or `unseen'.  If ITEMS is a list of symbols, a list of values is
returned, if ITEMS is a symbol only its value is returned."
  (with-current-buffer (or buffer (current-buffer))
    (when (imap-ok-p
	   (imap-send-command-wait (list "STATUS \""
					 (imap-utf7-encode mailbox)
					 "\" "
					 (upcase
					  (format "%s"
						  (if (listp items)
						      items
						    (list items)))))))
      (if (listp items)
	  (mapcar (lambda (item)
		    (imap-mailbox-get item mailbox))
		  items)
	(imap-mailbox-get items mailbox)))))

(defun imap-mailbox-status-asynch (mailbox items &optional buffer)
  "Send status item request ITEM on MAILBOX to server in BUFFER.
ITEMS can be a symbol or a list of symbols, valid symbols are one of
the STATUS data items -- i.e. 'messages, 'recent, 'uidnext, 'uidvalidity
or 'unseen.  The IMAP command tag is returned."
  (with-current-buffer (or buffer (current-buffer))
    (imap-send-command (list "STATUS \""
			     (imap-utf7-encode mailbox)
			     "\" "
			     (upcase
			      (format "%s"
				      (if (listp items)
					  items
					(list items))))))))

(defun imap-mailbox-acl-get (&optional mailbox buffer)
  "Get ACL on MAILBOX from server in BUFFER."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (when (imap-ok-p
	     (imap-send-command-wait (list "GETACL \""
					   (or mailbox imap-current-mailbox)
					   "\"")))
	(imap-mailbox-get-1 'acl (or mailbox imap-current-mailbox))))))

(defun imap-mailbox-acl-set (identifier rights &optional mailbox buffer)
  "Change/set ACL for IDENTIFIER to RIGHTS in MAILBOX from server in BUFFER."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "SETACL \""
				     (or mailbox imap-current-mailbox)
				     "\" "
				     identifier
				     " "
				     rights))))))

(defun imap-mailbox-acl-delete (identifier &optional mailbox buffer)
  "Remove any <identifier,rights> pair for IDENTIFIER in MAILBOX from server in BUFFER."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p
       (imap-send-command-wait (list "DELETEACL \""
				     (or mailbox imap-current-mailbox)
				     "\" "
				     identifier))))))


;; Message functions:

(defun imap-current-message (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    imap-current-message))

(defun imap-list-to-message-set (list)
  (mapconcat (lambda (item)
	       (number-to-string item))
	     (if (listp list)
		 list
	       (list list))
	     ","))

(defun imap-range-to-message-set (range)
  (mapconcat
   (lambda (item)
     (if (consp item)
	 (format "%d:%d"
		 (car item) (cdr item))
       (format "%d" item)))
   (if (and (listp range) (not (listp (cdr range))))
       (list range) ;; make (1 . 2) into ((1 . 2))
     range)
   ","))

(defun imap-fetch-asynch (uids props &optional nouidfetch buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-send-command (format "%sFETCH %s %s" (if nouidfetch "" "UID ")
			       (if (listp uids)
				   (imap-list-to-message-set uids)
				 uids)
			       props))))

(defun imap-fetch (uids props &optional receive nouidfetch buffer)
  "Fetch properties PROPS from message set UIDS from server in BUFFER.
UIDS can be a string, number or a list of numbers.  If RECEIVE
is non-nil return these properties."
  (with-current-buffer (or buffer (current-buffer))
    (when (imap-ok-p (imap-send-command-wait
		      (format "%sFETCH %s %s" (if nouidfetch "" "UID ")
			      (if (listp uids)
				  (imap-list-to-message-set uids)
				uids)
			      props)))
      (if (or (null receive) (stringp uids))
	  t
	(if (listp uids)
	    (mapcar (lambda (uid)
		      (if (listp receive)
			  (mapcar (lambda (prop)
				    (imap-message-get uid prop))
				  receive)
			(imap-message-get uid receive)))
		    uids)
	  (imap-message-get uids receive))))))

(defun imap-message-put (uid propname value &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if imap-message-data
	(put (intern (number-to-string uid) imap-message-data)
	     propname value)
      (error "Imap-message-data is nil, uid %s prop %s value %s buffer %s"
	     uid propname value (current-buffer)))
    t))

(defun imap-message-get (uid propname &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (get (intern-soft (number-to-string uid) imap-message-data)
	 propname)))

(defun imap-message-map (func propname &optional buffer)
  "Map a function across each message in `imap-message-data', returning a list."
  (with-current-buffer (or buffer (current-buffer))
    (let (result)
      (mapatoms
       (lambda (s)
	 (push (funcall func (get s 'UID) (get s propname)) result))
       imap-message-data)
      result)))

(defmacro imap-message-envelope-date (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 0)))

(defmacro imap-message-envelope-subject (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 1)))

(defmacro imap-message-envelope-from (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 2)))

(defmacro imap-message-envelope-sender (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 3)))

(defmacro imap-message-envelope-reply-to (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 4)))

(defmacro imap-message-envelope-to (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 5)))

(defmacro imap-message-envelope-cc (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 6)))

(defmacro imap-message-envelope-bcc (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 7)))

(defmacro imap-message-envelope-in-reply-to (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 8)))

(defmacro imap-message-envelope-message-id (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (elt (imap-message-get ,uid 'ENVELOPE) 9)))

(defmacro imap-message-body (uid &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (imap-message-get ,uid 'BODY)))

;; FIXME: Should this try to use CHARSET?  -- fx
(defun imap-search (predicate &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-mailbox-put 'search 'dummy)
    (when (imap-ok-p (imap-send-command-wait (concat "UID SEARCH " predicate)))
      (if (eq (imap-mailbox-get-1 'search imap-current-mailbox) 'dummy)
	  (progn
	    (message "Missing SEARCH response to a SEARCH command (server not RFC compliant)...")
	    nil)
	(imap-mailbox-get-1 'search imap-current-mailbox)))))

(defun imap-message-flag-permanent-p (flag &optional mailbox buffer)
  "Return t if FLAG can be permanently (between IMAP sessions) saved on articles, in MAILBOX on server in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (member "\\*" (imap-mailbox-get 'permanentflags mailbox))
	(member flag (imap-mailbox-get 'permanentflags mailbox)))))

(defun imap-message-flags-set (articles flags &optional silent buffer)
  (when (and articles flags)
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p (imap-send-command-wait
		  (concat "UID STORE " articles
			  " FLAGS" (if silent ".SILENT") " (" flags ")"))))))

(defun imap-message-flags-del (articles flags &optional silent buffer)
  (when (and articles flags)
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p (imap-send-command-wait
		  (concat "UID STORE " articles
			  " -FLAGS" (if silent ".SILENT") " (" flags ")"))))))

(defun imap-message-flags-add (articles flags &optional silent buffer)
  (when (and articles flags)
    (with-current-buffer (or buffer (current-buffer))
      (imap-ok-p (imap-send-command-wait
		  (concat "UID STORE " articles
			  " +FLAGS" (if silent ".SILENT") " (" flags ")"))))))

;; Cf. http://thread.gmane.org/gmane.emacs.gnus.general/65317/focus=65343
;; Signal an error if we'd get an integer overflow.
;;
;; FIXME: Identify relevant calls to `string-to-number' and replace them with
;; `imap-string-to-integer'.
(defun imap-string-to-integer (string &optional base)
  (let ((number (string-to-number string base)))
    (if (> number most-positive-fixnum)
	(error
	 (format "String %s cannot be converted to a Lisp integer" number))
      number)))

(defun imap-fetch-safe (uids props &optional receive nouidfetch buffer)
  "Like `imap-fetch', but DTRT with Exchange 2007 bug.
However, UIDS here is a cons, where the car is the canonical form
of the UIDS specification, and the cdr is the one which works with
Exchange 2007 or, potentially, other buggy servers.
See `imap-enable-exchange-bug-workaround'."
  ;; The first time we get here for a given, we'll try the canonical
  ;; form.  If we get the known error from the buggy server, set the
  ;; flag buffer-locally (to account for connections to multiple
  ;; servers), then re-try with the alternative UIDS spec.  We don't
  ;; unconditionally use the alternative form, since the
  ;; currently-used alternatives are seriously inefficient with some
  ;; servers (although they are valid).
  ;;
  ;; FIXME:  Maybe it would be cleaner to have a flag to not signal
  ;; the error (which otherwise gives a message), and test
  ;; `imap-failed-tags'.  Also, Other IMAP clients use other forms of
  ;; request which work with Exchange, e.g. Claws does "UID FETCH 1:*
  ;; (UID)" rather than "FETCH UID 1,*".  Is there a good reason not
  ;; to do the same?
  (condition-case data
      ;; Binding `debug-on-error' allows us to get the error from
      ;; `imap-parse-response' -- it's normally caught by Emacs around
      ;; execution of a process filter.
      (let ((debug-on-error t))
	(imap-fetch (if imap-enable-exchange-bug-workaround
			(cdr uids)
		      (car uids))
		    props receive nouidfetch buffer))
    (error
     (if (and (not imap-enable-exchange-bug-workaround)
	      ;; This is the Exchange 2007 response.  It may be more
	      ;; robust just to check for a BAD response to the
	      ;; attempted fetch.
	      (string-match "The specified message set is invalid"
			    (cadr data)))
	 (with-current-buffer (or buffer (current-buffer))
	   (set (make-local-variable 'imap-enable-exchange-bug-workaround)
		t)
	   (imap-fetch (cdr uids) props receive nouidfetch))
       (signal (car data) (cdr data))))))

(defun imap-message-copyuid-1 (mailbox)
  (if (imap-capability 'UIDPLUS)
      (list (nth 0 (imap-mailbox-get-1 'copyuid mailbox))
	    (string-to-number (nth 2 (imap-mailbox-get-1 'copyuid mailbox))))
    (let ((old-mailbox imap-current-mailbox)
	  (state imap-state)
	  (imap-message-data (make-vector 2 0)))
      (when (imap-mailbox-examine-1 mailbox)
	(prog1
	    (and (imap-fetch-safe '("*" . "*:*") "UID")
		 (list (imap-mailbox-get-1 'uidvalidity mailbox)
		       (apply 'max (imap-message-map
				    (lambda (uid prop) uid) 'UID))))
	  (if old-mailbox
	      (imap-mailbox-select old-mailbox (eq state 'examine))
	    (imap-mailbox-unselect)))))))

(defun imap-message-copyuid (mailbox &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-message-copyuid-1 (imap-utf7-decode mailbox))))

(defun imap-message-copy (articles mailbox
				   &optional dont-create no-copyuid buffer)
  "Copy ARTICLES to MAILBOX on server in BUFFER.
ARTICLES is a string message set.  Create mailbox if it doesn't exist,
unless DONT-CREATE is non-nil.  On success, return a list with
the UIDVALIDITY of the mailbox the article(s) was copied to as the
first element.  The rest of list contains the saved articles' UIDs."
  (when articles
    (with-current-buffer (or buffer (current-buffer))
      (let ((mailbox (imap-utf7-encode mailbox)))
	(if (let ((cmd (concat "UID COPY " articles " \"" mailbox "\""))
		  (imap-current-target-mailbox mailbox))
	      (if (imap-ok-p (imap-send-command-wait cmd))
		  t
		(when (and (not dont-create)
			   ;; removed because of buggy Oracle server
			   ;; that doesn't send TRYCREATE tags (which
			   ;; is a MUST according to specifications):
			   ;;(imap-mailbox-get-1 'trycreate mailbox)
			   (imap-mailbox-create-1 mailbox))
		  (imap-ok-p (imap-send-command-wait cmd)))))
	    (or no-copyuid
		(imap-message-copyuid-1 mailbox)))))))

;; FIXME: Amalgamate with imap-message-copyuid-1, using an extra arg, since it
;; shares most of the code?  -- fx
(defun imap-message-appenduid-1 (mailbox)
  (if (imap-capability 'UIDPLUS)
      (imap-mailbox-get-1 'appenduid mailbox)
    (let ((old-mailbox imap-current-mailbox)
	  (state imap-state)
	  (imap-message-data (make-vector 2 0)))
      (when (imap-mailbox-examine-1 mailbox)
	(prog1
	    (and (imap-fetch-safe '("*" . "*:*") "UID")
		 (list (imap-mailbox-get-1 'uidvalidity mailbox)
		       (apply 'max (imap-message-map
				    (lambda (uid prop) uid) 'UID))))
	  (if old-mailbox
	      (imap-mailbox-select old-mailbox (eq state 'examine))
	    (imap-mailbox-unselect)))))))

(defun imap-message-appenduid (mailbox &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (imap-message-appenduid-1 (imap-utf7-encode mailbox))))

(defun imap-message-append (mailbox article &optional flags date-time buffer)
  "Append ARTICLE (a buffer) to MAILBOX on server in BUFFER.
FLAGS and DATE-TIME is currently not used.  Return a cons holding
uidvalidity of MAILBOX and UID the newly created article got, or nil
on failure."
  (let ((mailbox (imap-utf7-encode mailbox)))
    (with-current-buffer (or buffer (current-buffer))
      (and (let ((imap-current-target-mailbox mailbox))
	     (imap-ok-p
	      (imap-send-command-wait
	       (list "APPEND \"" mailbox "\" "  article))))
	   (imap-message-appenduid-1 mailbox)))))

(defun imap-body-lines (body)
  "Return number of lines in article by looking at the mime bodystructure BODY."
  (if (listp body)
      (if (stringp (car body))
	  (cond ((and (string= (upcase (car body)) "TEXT")
		      (numberp (nth 7 body)))
		 (nth 7 body))
		((and (string= (upcase (car body)) "MESSAGE")
		      (numberp (nth 9 body)))
		 (nth 9 body))
		(t 0))
	(apply '+ (mapcar 'imap-body-lines body)))
    0))

(defun imap-envelope-from (from)
  "Return a from string line."
  (and from
       (concat (aref from 0)
	       (if (aref from 0) " <")
	       (aref from 2)
	       "@"
	       (aref from 3)
	       (if (aref from 0) ">"))))


;; Internal functions.

(defun imap-add-callback (tag func)
  (setq imap-callbacks (append (list (cons tag func)) imap-callbacks)))

(defun imap-send-command-1 (cmdstr)
  (setq cmdstr (concat cmdstr imap-client-eol))
  (imap-log cmdstr)
  (process-send-string imap-process cmdstr))

(defun imap-send-command (command &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (not (listp command)) (setq command (list command)))
    (let ((tag (setq imap-tag (1+ imap-tag)))
	  cmd cmdstr)
      (setq cmdstr (concat (number-to-string imap-tag) " "))
      (while (setq cmd (pop command))
	(cond ((stringp cmd)
	       (setq cmdstr (concat cmdstr cmd)))
	      ((bufferp cmd)
	       (let ((eol imap-client-eol)
		     (calcfirst imap-calculate-literal-size-first)
		     size)
		 (with-current-buffer cmd
		   (if calcfirst
		       (setq size (buffer-size)))
		   (when (not (equal eol "\r\n"))
		     ;; XXX modifies buffer!
		     (goto-char (point-min))
		     (while (search-forward "\r\n" nil t)
		       (replace-match eol)))
		   (if (not calcfirst)
		       (setq size (buffer-size))))
		 (setq cmdstr
		       (concat cmdstr (format "{%d}" size))))
	       (unwind-protect
		   (progn
		     (imap-send-command-1 cmdstr)
		     (setq cmdstr nil)
		     (if (not (eq (imap-wait-for-tag tag) 'INCOMPLETE))
			 (setq command nil) ;; abort command if no cont-req
		       (let ((process imap-process)
			     (stream imap-stream)
			     (eol imap-client-eol))
			 (with-current-buffer cmd
			   (imap-log cmd)
			   (process-send-region process (point-min)
						(point-max)))
			 (process-send-string process imap-client-eol))))
		 (setq imap-continuation nil)))
	      ((functionp cmd)
	       (imap-send-command-1 cmdstr)
	       (setq cmdstr nil)
	       (unwind-protect
		   (setq command
			 (if (not (eq (imap-wait-for-tag tag) 'INCOMPLETE))
			     nil ;; abort command if no cont-req
			   (cons (funcall cmd imap-continuation)
				 command)))
		 (setq imap-continuation nil)))
	      (t
	       (error "Unknown command type"))))
      (if cmdstr
	  (imap-send-command-1 cmdstr))
      tag)))

(defun imap-wait-for-tag (tag &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let (imap-have-messaged)
      (while (and (null imap-continuation)
		  (memq (process-status imap-process) '(open run))
		  (< imap-reached-tag tag))
	(let ((len (/ (buffer-size) 1024))
	      message-log-max)
	  (unless (< len 10)
	    (setq imap-have-messaged t)
	    (message "imap read: %dk" len))
	  (accept-process-output imap-process
				 (truncate imap-read-timeout)
				 (truncate (* (- imap-read-timeout
						 (truncate imap-read-timeout))
					      1000)))))
      ;; A process can die _before_ we have processed everything it
      ;; has to say.  Moreover, this can happen in between the call to
      ;; accept-process-output and the call to process-status in an
      ;; iteration of the loop above.
      (when (and (null imap-continuation)
		 (< imap-reached-tag tag))
	(accept-process-output imap-process 0 0))
      (when imap-have-messaged
	(message ""))
      (and (memq (process-status imap-process) '(open run))
	   (or (assq tag imap-failed-tags)
	       (if imap-continuation
		   'INCOMPLETE
		 'OK))))))

(defun imap-sentinel (process string)
  (delete-process process))

(defun imap-find-next-line ()
  "Return point at end of current line, taking into account literals.
Return nil if no complete line has arrived."
  (when (re-search-forward (concat imap-server-eol "\\|{\\([0-9]+\\)}"
				   imap-server-eol)
			   nil t)
    (if (match-string 1)
	(if (< (point-max) (+ (point) (string-to-number (match-string 1))))
	    nil
	  (goto-char (+ (point) (string-to-number (match-string 1))))
	  (imap-find-next-line))
      (point))))

(defun imap-arrival-filter (proc string)
  "IMAP process filter."
  ;; Sometimes, we are called even though the process has died.
  ;; Better abstain from doing stuff in that case.
  (when (buffer-name (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string)
      (imap-log string)
      (let (end)
	(goto-char (point-min))
	(while (setq end (imap-find-next-line))
	  (save-restriction
	    (narrow-to-region (point-min) end)
	    (delete-char (- (length imap-server-eol)))
	    (goto-char (point-min))
	    (unwind-protect
		(cond ((eq imap-state 'initial)
		       (imap-parse-greeting))
		      ((or (eq imap-state 'auth)
			   (eq imap-state 'nonauth)
			   (eq imap-state 'selected)
			   (eq imap-state 'examine))
		       (imap-parse-response))
		      (t
		       (message "Unknown state %s in arrival filter"
				imap-state)))
	      (delete-region (point-min) (point-max)))))))))


;; Imap parser.

(defsubst imap-forward ()
  (or (eobp) (forward-char)))

;;   number          = 1*DIGIT
;;                       ; Unsigned 32-bit integer
;;                       ; (0 <= n < 4,294,967,296)

(defsubst imap-parse-number ()
  (when (looking-at "[0-9]+")
    (prog1
	(string-to-number (match-string 0))
      (goto-char (match-end 0)))))

;;   literal         = "{" number "}" CRLF *CHAR8
;;                       ; Number represents the number of CHAR8s

(defsubst imap-parse-literal ()
  (when (looking-at "{\\([0-9]+\\)}\r\n")
    (let ((pos (match-end 0))
	  (len (string-to-number (match-string 1))))
      (if (< (point-max) (+ pos len))
	  nil
	(goto-char (+ pos len))
	(buffer-substring pos (+ pos len))))))

;;   string          = quoted / literal
;;
;;   quoted          = DQUOTE *QUOTED-CHAR DQUOTE
;;
;;   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
;;                     "\" quoted-specials
;;
;;   quoted-specials = DQUOTE / "\"
;;
;;   TEXT-CHAR       = <any CHAR except CR and LF>

(defsubst imap-parse-string ()
  (cond ((eq (char-after) ?\")
	 (forward-char 1)
	 (let ((p (point)) (name ""))
	   (skip-chars-forward "^\"\\\\")
	   (setq name (buffer-substring p (point)))
	   (while (eq (char-after) ?\\)
	     (setq p (1+ (point)))
	     (forward-char 2)
	     (skip-chars-forward "^\"\\\\")
	     (setq name (concat name (buffer-substring p (point)))))
	   (forward-char 1)
	   name))
	((eq (char-after) ?{)
	 (imap-parse-literal))))

;;   nil             = "NIL"

(defsubst imap-parse-nil ()
  (if (looking-at "NIL")
      (goto-char (match-end 0))))

;;   nstring         = string / nil

(defsubst imap-parse-nstring ()
  (or (imap-parse-string)
      (and (imap-parse-nil)
	   nil)))

;;   astring         = atom / string
;;
;;   atom            = 1*ATOM-CHAR
;;
;;   ATOM-CHAR       = <any CHAR except atom-specials>
;;
;;   atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
;;                     quoted-specials
;;
;;   list-wildcards  = "%" / "*"
;;
;;   quoted-specials = DQUOTE / "\"

(defsubst imap-parse-astring ()
  (or (imap-parse-string)
      (buffer-substring (point)
			(if (re-search-forward "[(){ \r\n%*\"\\]" nil t)
			    (goto-char (1- (match-end 0)))
			  (end-of-line)
			  (point)))))

;;   address         = "(" addr-name SP addr-adl SP addr-mailbox SP
;;                      addr-host ")"
;;
;;   addr-adl        = nstring
;;                       ; Holds route from [RFC-822] route-addr if
;;                       ; non-nil
;;
;;   addr-host       = nstring
;;                       ; nil indicates [RFC-822] group syntax.
;;                       ; Otherwise, holds [RFC-822] domain name
;;
;;   addr-mailbox    = nstring
;;                       ; nil indicates end of [RFC-822] group; if
;;                       ; non-nil and addr-host is nil, holds
;;                       ; [RFC-822] group name.
;;                       ; Otherwise, holds [RFC-822] local-part
;;                       ; after removing [RFC-822] quoting
;;
;;   addr-name       = nstring
;;                       ; If non-nil, holds phrase from [RFC-822]
;;                       ; mailbox after removing [RFC-822] quoting
;;

(defsubst imap-parse-address ()
  (let (address)
    (when (eq (char-after) ?\()
      (imap-forward)
      (setq address (vector (prog1 (imap-parse-nstring)
			      (imap-forward))
			    (prog1 (imap-parse-nstring)
			      (imap-forward))
			    (prog1 (imap-parse-nstring)
			      (imap-forward))
			    (imap-parse-nstring)))
      (when (eq (char-after) ?\))
	(imap-forward)
	address))))

;;   address-list    = "(" 1*address ")" / nil
;;
;;   nil             = "NIL"

(defsubst imap-parse-address-list ()
  (if (eq (char-after) ?\()
      (let (address addresses)
	(imap-forward)
	(while (and (not (eq (char-after) ?\)))
		    ;; next line for MS Exchange bug
		    (progn (and (eq (char-after) ? ) (imap-forward)) t)
		    (setq address (imap-parse-address)))
	  (setq addresses (cons address addresses)))
	(when (eq (char-after) ?\))
	  (imap-forward)
	  (nreverse addresses)))
    ;; With assert, the code might not be eval'd.
    ;; (assert (imap-parse-nil) t "In imap-parse-address-list")
    (imap-parse-nil)))

;;   mailbox         = "INBOX" / astring
;;                       ; INBOX is case-insensitive.  All case variants of
;;                       ; INBOX (e.g. "iNbOx") MUST be interpreted as INBOX
;;                       ; not as an astring.  An astring which consists of
;;                       ; the case-insensitive sequence "I" "N" "B" "O" "X"
;;                       ; is considered to be INBOX and not an astring.
;;                       ;  Refer to section 5.1 for further
;;                       ; semantic details of mailbox names.

(defsubst imap-parse-mailbox ()
  (let ((mailbox (imap-parse-astring)))
    (if (string-equal "INBOX" (upcase mailbox))
	"INBOX"
      mailbox)))

;;   greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF
;;
;;   resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text
;;                       ; Authentication condition
;;
;;   resp-cond-bye   = "BYE" SP resp-text

(defun imap-parse-greeting ()
  "Parse an IMAP greeting."
  (cond ((looking-at "\\* OK ")
	 (setq imap-state 'nonauth))
	((looking-at "\\* PREAUTH ")
	 (setq imap-state 'auth))
	((looking-at "\\* BYE ")
	 (setq imap-state 'closed))))

;;   response        = *(continue-req / response-data) response-done
;;
;;   continue-req    = "+" SP (resp-text / base64) CRLF
;;
;;   response-data   = "*" SP (resp-cond-state / resp-cond-bye /
;;                     mailbox-data / message-data / capability-data) CRLF
;;
;;   response-done   = response-tagged / response-fatal
;;
;;   response-fatal  = "*" SP resp-cond-bye CRLF
;;                       ; Server closes connection immediately
;;
;;   response-tagged = tag SP resp-cond-state CRLF
;;
;;   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
;;                       ; Status condition
;;
;;   resp-cond-bye   = "BYE" SP resp-text
;;
;;   mailbox-data    =  "FLAGS" SP flag-list /
;;		        "LIST" SP mailbox-list /
;;                      "LSUB" SP mailbox-list /
;;		        "SEARCH" *(SP nz-number) /
;;                      "STATUS" SP mailbox SP "("
;;	                      [status-att SP number *(SP status-att SP number)] ")" /
;;                      number SP "EXISTS" /
;;		        number SP "RECENT"
;;
;;   message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))
;;
;;   capability-data = "CAPABILITY" *(SP capability) SP "IMAP4rev1"
;;                     *(SP capability)
;;                       ; IMAP4rev1 servers which offer RFC 1730
;;                       ; compatibility MUST list "IMAP4" as the first
;;                       ; capability.

(defun imap-parse-response ()
  "Parse a IMAP command response."
  (let (token)
    (case (setq token (read (current-buffer)))
      (+ (setq imap-continuation
	       (or (buffer-substring (min (point-max) (1+ (point)))
				     (point-max))
		   t)))
      (* (case (prog1 (setq token (read (current-buffer)))
		 (imap-forward))
	   (OK         (imap-parse-resp-text))
	   (NO         (imap-parse-resp-text))
	   (BAD        (imap-parse-resp-text))
	   (BYE        (imap-parse-resp-text))
	   (FLAGS      (imap-mailbox-put 'flags (imap-parse-flag-list)))
	   (LIST       (imap-parse-data-list 'list))
	   (LSUB       (imap-parse-data-list 'lsub))
	   (SEARCH     (imap-mailbox-put
			'search
			(read (concat "(" (buffer-substring (point) (point-max)) ")"))))
	   (STATUS     (imap-parse-status))
	   (CAPABILITY (setq imap-capability
			       (read (concat "(" (upcase (buffer-substring
							  (point) (point-max)))
					     ")"))))
	   (ID	       (setq imap-id (read (buffer-substring (point)
							     (point-max)))))
	   (ACL        (imap-parse-acl))
	   (t       (case (prog1 (read (current-buffer))
			    (imap-forward))
		      (EXISTS  (imap-mailbox-put 'exists token))
		      (RECENT  (imap-mailbox-put 'recent token))
		      (EXPUNGE t)
		      (FETCH   (imap-parse-fetch token))
		      (t       (message "Garbage: %s" (buffer-string)))))))
      (t (let (status)
	   (if (not (integerp token))
	       (message "Garbage: %s" (buffer-string))
	     (case (prog1 (setq status (read (current-buffer)))
		     (imap-forward))
	       (OK  (progn
		      (setq imap-reached-tag (max imap-reached-tag token))
		      (imap-parse-resp-text)))
	       (NO  (progn
		      (setq imap-reached-tag (max imap-reached-tag token))
		      (save-excursion
			(imap-parse-resp-text))
		      (let (code text)
			(when (eq (char-after) ?\[)
			  (setq code (buffer-substring (point)
						       (search-forward "]")))
			  (imap-forward))
			(setq text (buffer-substring (point) (point-max)))
			(push (list token status code text)
			      imap-failed-tags))))
	       (BAD (progn
		      (setq imap-reached-tag (max imap-reached-tag token))
		      (save-excursion
			(imap-parse-resp-text))
		      (let (code text)
			(when (eq (char-after) ?\[)
			  (setq code (buffer-substring (point)
						       (search-forward "]")))
			  (imap-forward))
			(setq text (buffer-substring (point) (point-max)))
			(push (list token status code text) imap-failed-tags)
			(error "Internal error, tag %s status %s code %s text %s"
			       token status code text))))
	       (t   (message "Garbage: %s" (buffer-string))))
	     (when (assq token imap-callbacks)
	       (funcall (cdr (assq token imap-callbacks)) token status)
	       (setq imap-callbacks
		     (imap-remassoc token imap-callbacks)))))))))

;;   resp-text       = ["[" resp-text-code "]" SP] text
;;
;;   text            = 1*TEXT-CHAR
;;
;;   TEXT-CHAR       = <any CHAR except CR and LF>

(defun imap-parse-resp-text ()
  (imap-parse-resp-text-code))

;;   resp-text-code  = "ALERT" /
;;                     "BADCHARSET [SP "(" astring *(SP astring) ")" ] /
;;                     "NEWNAME" SP string SP string /
;;		       "PARSE" /
;;                     "PERMANENTFLAGS" SP "("
;;                               [flag-perm *(SP flag-perm)] ")" /
;;                     "READ-ONLY" /
;;		       "READ-WRITE" /
;;		       "TRYCREATE" /
;;                     "UIDNEXT" SP nz-number /
;;		       "UIDVALIDITY" SP nz-number /
;;                     "UNSEEN" SP nz-number /
;;                     resp-text-atom [SP 1*<any TEXT-CHAR except "]">]
;;
;;   resp_code_apnd  = "APPENDUID" SPACE nz_number SPACE uniqueid
;;
;;   resp_code_copy  = "COPYUID" SPACE nz_number SPACE set SPACE set
;;
;;   set             = sequence-num / (sequence-num ":" sequence-num) /
;;                        (set "," set)
;;                          ; Identifies a set of messages.  For message
;;                          ; sequence numbers, these are consecutive
;;                          ; numbers from 1 to the number of messages in
;;                          ; the mailbox
;;                          ; Comma delimits individual numbers, colon
;;                          ; delimits between two numbers inclusive.
;;                          ; Example: 2,4:7,9,12:* is 2,4,5,6,7,9,12,13,
;;                          ; 14,15 for a mailbox with 15 messages.
;;
;;   sequence-num    = nz-number / "*"
;;                          ; * is the largest number in use.  For message
;;                          ; sequence numbers, it is the number of messages
;;                          ; in the mailbox.  For unique identifiers, it is
;;                          ; the unique identifier of the last message in
;;                          ; the mailbox.
;;
;;   flag-perm       = flag / "\*"
;;
;;   flag            = "\Answered" / "\Flagged" / "\Deleted" /
;;                     "\Seen" / "\Draft" / flag-keyword / flag-extension
;;                       ; Does not include "\Recent"
;;
;;   flag-extension  = "\" atom
;;                       ; Future expansion.  Client implementations
;;                       ; MUST accept flag-extension flags.  Server
;;                       ; implementations MUST NOT generate
;;                       ; flag-extension flags except as defined by
;;                       ; future standard or standards-track
;;                       ; revisions of this specification.
;;
;;   flag-keyword    = atom
;;
;;   resp-text-atom  = 1*<any ATOM-CHAR except "]">

(defun imap-parse-resp-text-code ()
  ;; xxx next line for stalker communigate pro 3.3.1 bug
  (when (looking-at " \\[")
    (imap-forward))
  (when (eq (char-after) ?\[)
    (imap-forward)
    (cond ((search-forward "PERMANENTFLAGS " nil t)
	   (imap-mailbox-put 'permanentflags (imap-parse-flag-list)))
	  ((search-forward "UIDNEXT \\([0-9]+\\)" nil t)
	   (imap-mailbox-put 'uidnext (match-string 1)))
	  ((search-forward "UNSEEN " nil t)
	   (imap-mailbox-put 'first-unseen (read (current-buffer))))
	  ((looking-at "UIDVALIDITY \\([0-9]+\\)")
	   (imap-mailbox-put 'uidvalidity (match-string 1)))
	  ((search-forward "READ-ONLY" nil t)
	   (imap-mailbox-put 'read-only t))
	  ((search-forward "NEWNAME " nil t)
	   (let (oldname newname)
	     (setq oldname (imap-parse-string))
	     (imap-forward)
	     (setq newname (imap-parse-string))
	     (imap-mailbox-put 'newname newname oldname)))
	  ((search-forward "TRYCREATE" nil t)
	   (imap-mailbox-put 'trycreate t imap-current-target-mailbox))
	  ((looking-at "APPENDUID \\([0-9]+\\) \\([0-9]+\\)")
	   (imap-mailbox-put 'appenduid
			     (list (match-string 1)
				   (string-to-number (match-string 2)))
			     imap-current-target-mailbox))
	  ((looking-at "COPYUID \\([0-9]+\\) \\([0-9,:]+\\) \\([0-9,:]+\\)")
	   (imap-mailbox-put 'copyuid (list (match-string 1)
					    (match-string 2)
					    (match-string 3))
			     imap-current-target-mailbox))
	  ((search-forward "ALERT] " nil t)
	   (message "Imap server %s information: %s" imap-server
		    (buffer-substring (point) (point-max)))))))

;;   mailbox-list    = "(" [mbx-list-flags] ")" SP
;;                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
;;
;;   mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag
;;                     *(SP mbx-list-oflag) /
;;                     mbx-list-oflag *(SP mbx-list-oflag)
;;
;;   mbx-list-oflag  = "\Noinferiors" / flag-extension
;;                       ; Other flags; multiple possible per LIST response
;;
;;   mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked"
;;                       ; Selectability flags; only one per LIST response
;;
;;   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
;;                     "\" quoted-specials
;;
;;   quoted-specials = DQUOTE / "\"

(defun imap-parse-data-list (type)
  (let (flags delimiter mailbox)
    (setq flags (imap-parse-flag-list))
    (when (looking-at " NIL\\| \"\\\\?\\(.\\)\"")
      (setq delimiter (match-string 1))
      (goto-char (1+ (match-end 0)))
      (when (setq mailbox (imap-parse-mailbox))
	(imap-mailbox-put type t mailbox)
	(imap-mailbox-put 'list-flags flags mailbox)
	(imap-mailbox-put 'delimiter delimiter mailbox)))))

;;  msg_att         ::= "(" 1#("ENVELOPE" SPACE envelope /
;;                      "FLAGS" SPACE "(" #(flag / "\Recent") ")" /
;;                      "INTERNALDATE" SPACE date_time /
;;                      "RFC822" [".HEADER" / ".TEXT"] SPACE nstring /
;;                      "RFC822.SIZE" SPACE number /
;;                      "BODY" ["STRUCTURE"] SPACE body /
;;                      "BODY" section ["<" number ">"] SPACE nstring /
;;                      "UID" SPACE uniqueid) ")"
;;
;;  date_time       ::= <"> date_day_fixed "-" date_month "-" date_year
;;                      SPACE time SPACE zone <">
;;
;;  section         ::= "[" [section_text / (nz_number *["." nz_number]
;;                      ["." (section_text / "MIME")])] "]"
;;
;;  section_text    ::= "HEADER" / "HEADER.FIELDS" [".NOT"]
;;                      SPACE header_list / "TEXT"
;;
;;  header_fld_name ::= astring
;;
;;  header_list     ::= "(" 1#header_fld_name ")"

(defsubst imap-parse-header-list ()
  (when (eq (char-after) ?\()
    (let (strlist)
      (while (not (eq (char-after) ?\)))
	(imap-forward)
	(push (imap-parse-astring) strlist))
      (imap-forward)
      (nreverse strlist))))

(defsubst imap-parse-fetch-body-section ()
  (let ((section
	 (buffer-substring (point) (1- (re-search-forward "[] ]" nil t)))))
    (if (eq (char-before) ? )
	(prog1
	    (mapconcat 'identity (cons section (imap-parse-header-list)) " ")
	  (search-forward "]" nil t))
      section)))

(defun imap-parse-fetch (response)
  (when (eq (char-after) ?\()
    (let (uid flags envelope internaldate rfc822 rfc822header rfc822text
	      rfc822size body bodydetail bodystructure flags-empty)
      ;; Courier can insert spurious blank characters which will
      ;; confuse `read', so skip past them.
      (while (let ((moved (skip-chars-forward " \t")))
	       (prog1 (not (eq (char-after) ?\)))
		 (unless (= moved 0) (backward-char))))
	(imap-forward)
	(let ((token (read (current-buffer))))
	  (imap-forward)
	  (cond ((eq token 'UID)
		 (setq uid (condition-case ()
			       (read (current-buffer))
			     (error))))
		((eq token 'FLAGS)
		 (setq flags (imap-parse-flag-list))
		 (if (not flags)
		     (setq flags-empty 't)))
		((eq token 'ENVELOPE)
		 (setq envelope (imap-parse-envelope)))
		((eq token 'INTERNALDATE)
		 (setq internaldate (imap-parse-string)))
		((eq token 'RFC822)
		 (setq rfc822 (imap-parse-nstring)))
		((eq token 'RFC822.HEADER)
		 (setq rfc822header (imap-parse-nstring)))
		((eq token 'RFC822.TEXT)
		 (setq rfc822text (imap-parse-nstring)))
		((eq token 'RFC822.SIZE)
		 (setq rfc822size (read (current-buffer))))
		((eq token 'BODY)
		 (if (eq (char-before) ?\[)
		     (push (list
			    (upcase (imap-parse-fetch-body-section))
			    (and (eq (char-after) ?<)
				 (buffer-substring (1+ (point))
						   (search-forward ">" nil t)))
			    (progn (imap-forward)
				   (imap-parse-nstring)))
			   bodydetail)
		   (setq body (imap-parse-body))))
		((eq token 'BODYSTRUCTURE)
		 (setq bodystructure (imap-parse-body))))))
      (when uid
	(setq imap-current-message uid)
	(imap-message-put uid 'UID uid)
	(and (or flags flags-empty) (imap-message-put uid 'FLAGS flags))
	(and envelope (imap-message-put uid 'ENVELOPE envelope))
	(and internaldate (imap-message-put uid 'INTERNALDATE internaldate))
	(and rfc822 (imap-message-put uid 'RFC822 rfc822))
	(and rfc822header (imap-message-put uid 'RFC822.HEADER rfc822header))
	(and rfc822text (imap-message-put uid 'RFC822.TEXT rfc822text))
	(and rfc822size (imap-message-put uid 'RFC822.SIZE rfc822size))
	(and body (imap-message-put uid 'BODY body))
	(and bodydetail (imap-message-put uid 'BODYDETAIL bodydetail))
	(and bodystructure (imap-message-put uid 'BODYSTRUCTURE bodystructure))
	(run-hooks 'imap-fetch-data-hook)))))

;;   mailbox-data    =  ...
;;                      "STATUS" SP mailbox SP "("
;;	                      [status-att SP number
;;                            *(SP status-att SP number)] ")"
;;                      ...
;;
;;   status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
;;                     "UNSEEN"

(defun imap-parse-status ()
  (let ((mailbox (imap-parse-mailbox)))
    (if (eq (char-after) ? )
	(forward-char))
    (when (and mailbox (eq (char-after) ?\())
      (while (and (not (eq (char-after) ?\)))
		  (or (forward-char) t)
		  (looking-at "\\([A-Za-z]+\\) "))
	(let ((token (upcase (match-string 1))))
	  (goto-char (match-end 0))
	  (cond ((string= token "MESSAGES")
		 (imap-mailbox-put 'messages (read (current-buffer)) mailbox))
		((string= token "RECENT")
		 (imap-mailbox-put 'recent (read (current-buffer)) mailbox))
		((string= token "UIDNEXT")
		 (and (looking-at "[0-9]+")
		      (imap-mailbox-put 'uidnext (match-string 0) mailbox)
		      (goto-char (match-end 0))))
		((string= token "UIDVALIDITY")
		 (and (looking-at "[0-9]+")
		      (imap-mailbox-put 'uidvalidity (match-string 0) mailbox)
		      (goto-char (match-end 0))))
		((string= token "UNSEEN")
		 (imap-mailbox-put 'unseen (read (current-buffer)) mailbox))
		(t
		 (message "Unknown status data %s in mailbox %s ignored"
			  token mailbox)
		 (read (current-buffer)))))))))

;;   acl_data        ::= "ACL" SPACE mailbox *(SPACE identifier SPACE
;;                        rights)
;;
;;   identifier      ::= astring
;;
;;   rights          ::= astring

(defun imap-parse-acl ()
  (let ((mailbox (imap-parse-mailbox))
	identifier rights acl)
    (while (eq (char-after) ?\ )
      (imap-forward)
      (setq identifier (imap-parse-astring))
      (imap-forward)
      (setq rights (imap-parse-astring))
      (setq acl (append acl (list (cons identifier rights)))))
    (imap-mailbox-put 'acl acl mailbox)))

;;   flag-list       = "(" [flag *(SP flag)] ")"
;;
;;   flag            = "\Answered" / "\Flagged" / "\Deleted" /
;;                     "\Seen" / "\Draft" / flag-keyword / flag-extension
;;                       ; Does not include "\Recent"
;;
;;   flag-keyword    = atom
;;
;;   flag-extension  = "\" atom
;;                       ; Future expansion.  Client implementations
;;                       ; MUST accept flag-extension flags.  Server
;;                       ; implementations MUST NOT generate
;;                       ; flag-extension flags except as defined by
;;                       ; future standard or standards-track
;;                       ; revisions of this specification.

(defun imap-parse-flag-list ()
  (let (flag-list start)
    (assert (eq (char-after) ?\() nil "In imap-parse-flag-list 1")
    (while (and (not (eq (char-after) ?\)))
		(setq start (progn
			      (imap-forward)
			      ;; next line for Courier IMAP bug.
			      (skip-chars-forward " ")
			      (point)))
		(> (skip-chars-forward "^ )" (point-at-eol)) 0))
      (push (buffer-substring start (point)) flag-list))
    (assert (eq (char-after) ?\)) nil "In imap-parse-flag-list 2")
    (imap-forward)
    (nreverse flag-list)))

;;   envelope        = "(" env-date SP env-subject SP env-from SP env-sender SP
;;                     env-reply-to SP env-to SP env-cc SP env-bcc SP
;;                     env-in-reply-to SP env-message-id ")"
;;
;;   env-bcc         = "(" 1*address ")" / nil
;;
;;   env-cc          = "(" 1*address ")" / nil
;;
;;   env-date        = nstring
;;
;;   env-from        = "(" 1*address ")" / nil
;;
;;   env-in-reply-to = nstring
;;
;;   env-message-id  = nstring
;;
;;   env-reply-to    = "(" 1*address ")" / nil
;;
;;   env-sender      = "(" 1*address ")" / nil
;;
;;   env-subject     = nstring
;;
;;   env-to          = "(" 1*address ")" / nil

(defun imap-parse-envelope ()
  (when (eq (char-after) ?\()
    (imap-forward)
    (vector (prog1 (imap-parse-nstring)	;; date
	      (imap-forward))
	    (prog1 (imap-parse-nstring)	;; subject
	      (imap-forward))
	    (prog1 (imap-parse-address-list) ;; from
	      (imap-forward))
	    (prog1 (imap-parse-address-list) ;; sender
	      (imap-forward))
	    (prog1 (imap-parse-address-list) ;; reply-to
	      (imap-forward))
	    (prog1 (imap-parse-address-list) ;; to
	      (imap-forward))
	    (prog1 (imap-parse-address-list) ;; cc
	      (imap-forward))
	    (prog1 (imap-parse-address-list) ;; bcc
	      (imap-forward))
	    (prog1 (imap-parse-nstring)	;; in-reply-to
	      (imap-forward))
	    (prog1 (imap-parse-nstring)	;; message-id
	      (imap-forward)))))

;;   body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil

(defsubst imap-parse-string-list ()
  (cond ((eq (char-after) ?\() ;; body-fld-param
	 (let (strlist str)
	   (imap-forward)
	   (while (setq str (imap-parse-string))
	     (push str strlist)
	     ;; buggy stalker communigate pro 3.0 doesn't print SPC
	     ;; between body-fld-param's sometimes
	     (or (eq (char-after) ?\")
		 (imap-forward)))
	   (nreverse strlist)))
	((imap-parse-nil)
	 nil)))

;;   body-extension  = nstring / number /
;;                      "(" body-extension *(SP body-extension) ")"
;;                       ; Future expansion.  Client implementations
;;                       ; MUST accept body-extension fields.  Server
;;                       ; implementations MUST NOT generate
;;                       ; body-extension fields except as defined by
;;                       ; future standard or standards-track
;;                       ; revisions of this specification.

(defun imap-parse-body-extension ()
  (if (eq (char-after) ?\()
      (let (b-e)
	(imap-forward)
	(push (imap-parse-body-extension) b-e)
	(while (eq (char-after) ?\ )
	  (imap-forward)
	  (push (imap-parse-body-extension) b-e))
	(assert (eq (char-after) ?\)) nil "In imap-parse-body-extension")
	(imap-forward)
	(nreverse b-e))
    (or (imap-parse-number)
	(imap-parse-nstring))))

;;   body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch
;;
;;   body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch

(defsubst imap-parse-body-ext ()
  (let (ext)
    (when (eq (char-after) ?\ )	;; body-fld-dsp
      (imap-forward)
      (let (dsp)
	(if (eq (char-after) ?\()
	    (progn
	      (imap-forward)
	      (push (imap-parse-string) dsp)
	      (imap-forward)
	      (push (imap-parse-string-list) dsp)
	      (imap-forward))
	  ;; With assert, the code might not be eval'd.
	  ;; (assert (imap-parse-nil) t "In imap-parse-body-ext")
	  (imap-parse-nil))
	(push (nreverse dsp) ext))
      (when (eq (char-after) ?\ ) ;; body-fld-lang
	(imap-forward)
	(if (eq (char-after) ?\()
	    (push (imap-parse-string-list) ext)
	  (push (imap-parse-nstring) ext))
	(while (eq (char-after) ?\ ) ;; body-extension
	  (imap-forward)
	  (setq ext (append (imap-parse-body-extension) ext)))))
    ext))

;;   body            = "(" body-type-1part / body-type-mpart ")"
;;
;;   body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch
;;
;;   body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
;;                     *(SP body-extension)]]
;;                       ; MUST NOT be returned on non-extensible
;;                       ; "BODY" fetch
;;
;;   body-fields     = body-fld-param SP body-fld-id SP body-fld-desc SP
;;                     body-fld-enc SP body-fld-octets
;;
;;   body-fld-desc   = nstring
;;
;;   body-fld-dsp    = "(" string SP body-fld-param ")" / nil
;;
;;   body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
;;                     "QUOTED-PRINTABLE") DQUOTE) / string
;;
;;   body-fld-id     = nstring
;;
;;   body-fld-lang   = nstring / "(" string *(SP string) ")"
;;
;;   body-fld-lines  = number
;;
;;   body-fld-md5    = nstring
;;
;;   body-fld-octets = number
;;
;;   body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
;;
;;   body-type-1part = (body-type-basic / body-type-msg / body-type-text)
;;                     [SP body-ext-1part]
;;
;;   body-type-basic = media-basic SP body-fields
;;                       ; MESSAGE subtype MUST NOT be "RFC822"
;;
;;   body-type-msg   = media-message SP body-fields SP envelope
;;                     SP body SP body-fld-lines
;;
;;   body-type-text  = media-text SP body-fields SP body-fld-lines
;;
;;   body-type-mpart = 1*body SP media-subtype
;;                     [SP body-ext-mpart]
;;
;;   media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" /
;;                     "MESSAGE" / "VIDEO") DQUOTE) / string) SP media-subtype
;;                       ; Defined in [MIME-IMT]
;;
;;   media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE
;;                      ; Defined in [MIME-IMT]
;;
;;   media-subtype   = string
;;                       ; Defined in [MIME-IMT]
;;
;;   media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
;;                       ; Defined in [MIME-IMT]

(defun imap-parse-body ()
  (let (body)
    (when (eq (char-after) ?\()
      (imap-forward)
      (if (eq (char-after) ?\()
	  (let (subbody)
	    (while (and (eq (char-after) ?\()
			(setq subbody (imap-parse-body)))
	      ;; buggy stalker communigate pro 3.0 inserts a SPC between
	      ;; parts in multiparts
	      (when (and (eq (char-after) ?\ )
			 (eq (char-after (1+ (point))) ?\())
		(imap-forward))
	      (push subbody body))
	    (imap-forward)
	    (push (imap-parse-string) body) ;; media-subtype
	    (when (eq (char-after) ?\ )	;; body-ext-mpart:
	      (imap-forward)
	      (if (eq (char-after) ?\()	;; body-fld-param
		  (push (imap-parse-string-list) body)
		(push (and (imap-parse-nil) nil) body))
	      (setq body
		    (append (imap-parse-body-ext) body))) ;; body-ext-...
	    (assert (eq (char-after) ?\)) nil "In imap-parse-body")
	    (imap-forward)
	    (nreverse body))

	(push (imap-parse-string) body)	;; media-type
	(imap-forward)
	(push (imap-parse-string) body)	;; media-subtype
	(imap-forward)
	;; next line for Sun SIMS bug
	(and (eq (char-after) ? ) (imap-forward))
	(if (eq (char-after) ?\() ;; body-fld-param
	    (push (imap-parse-string-list) body)
	  (push (and (imap-parse-nil) nil) body))
	(imap-forward)
	(push (imap-parse-nstring) body) ;; body-fld-id
	(imap-forward)
	(push (imap-parse-nstring) body) ;; body-fld-desc
	(imap-forward)
	;; Next `or' for Sun SIMS bug.  It regards body-fld-enc as a
	;; nstring and returns nil instead of defaulting back to 7BIT
	;; as the standard says.
	;; Exchange (2007, at least) does this as well.
	(push (or (imap-parse-nstring) "7BIT") body) ;; body-fld-enc
	(imap-forward)
	;; Exchange 2007 can return -1, contrary to the spec...
	(if (eq (char-after) ?-)
	    (progn
	      (skip-chars-forward "-0-9")
	      (push nil body))
	  (push (imap-parse-number) body)) ;; body-fld-octets

	;; Ok, we're done parsing the required parts, what comes now is one of
	;; three things:
	;;
	;; envelope       (then we're parsing body-type-msg)
	;; body-fld-lines (then we're parsing body-type-text)
	;; body-ext-1part (then we're parsing body-type-basic)
	;;
	;; The problem is that the two first are in turn optionally followed
	;; by the third.  So we parse the first two here (if there are any)...

	(when (eq (char-after) ?\ )
	  (imap-forward)
	  (let (lines)
	    (cond ((eq (char-after) ?\() ;; body-type-msg:
		   (push (imap-parse-envelope) body) ;; envelope
		   (imap-forward)
		   (push (imap-parse-body) body) ;; body
		   ;; buggy stalker communigate pro 3.0 doesn't print
		   ;; number of lines in message/rfc822 attachment
		   (if (eq (char-after) ?\))
		       (push 0 body)
		     (imap-forward)
		     (push (imap-parse-number) body))) ;; body-fld-lines
		  ((setq lines (imap-parse-number)) ;; body-type-text:
		   (push lines body)) ;; body-fld-lines
		  (t
		   (backward-char))))) ;; no match...

	;; ...and then parse the third one here...

	(when (eq (char-after) ?\ ) ;; body-ext-1part:
	  (imap-forward)
	  (push (imap-parse-nstring) body) ;; body-fld-md5
	  (setq body (append (imap-parse-body-ext) body))) ;; body-ext-1part..

	(assert (eq (char-after) ?\)) nil "In imap-parse-body 2")
	(imap-forward)
	(nreverse body)))))

(when imap-debug			; (untrace-all)
  (require 'trace)
  (buffer-disable-undo (get-buffer-create imap-debug-buffer))
  (mapc (lambda (f) (trace-function-background f imap-debug-buffer))
	'(
	  imap-utf7-encode
	  imap-utf7-decode
	  imap-error-text
	  imap-kerberos4s-p
	  imap-kerberos4-open
	  imap-ssl-p
	  imap-ssl-open
	  imap-network-p
	  imap-network-open
	  imap-interactive-login
	  imap-kerberos4a-p
	  imap-kerberos4-auth
	  imap-cram-md5-p
	  imap-cram-md5-auth
	  imap-login-p
	  imap-login-auth
	  imap-anonymous-p
	  imap-anonymous-auth
	  imap-open-1
	  imap-open
	  imap-opened
	  imap-ping-server
	  imap-authenticate
	  imap-close
	  imap-capability
	  imap-namespace
	  imap-send-command-wait
	  imap-mailbox-put
	  imap-mailbox-get
	  imap-mailbox-map-1
	  imap-mailbox-map
	  imap-current-mailbox
	  imap-current-mailbox-p-1
	  imap-current-mailbox-p
	  imap-mailbox-select-1
	  imap-mailbox-select
	  imap-mailbox-examine-1
	  imap-mailbox-examine
	  imap-mailbox-unselect
	  imap-mailbox-expunge
	  imap-mailbox-close
	  imap-mailbox-create-1
	  imap-mailbox-create
	  imap-mailbox-delete
	  imap-mailbox-rename
	  imap-mailbox-lsub
	  imap-mailbox-list
	  imap-mailbox-subscribe
	  imap-mailbox-unsubscribe
	  imap-mailbox-status
	  imap-mailbox-acl-get
	  imap-mailbox-acl-set
	  imap-mailbox-acl-delete
	  imap-current-message
	  imap-list-to-message-set
	  imap-fetch-asynch
	  imap-fetch
	  imap-fetch-safe
	  imap-message-put
	  imap-message-get
	  imap-message-map
	  imap-search
	  imap-message-flag-permanent-p
	  imap-message-flags-set
	  imap-message-flags-del
	  imap-message-flags-add
	  imap-message-copyuid-1
	  imap-message-copyuid
	  imap-message-copy
	  imap-message-appenduid-1
	  imap-message-appenduid
	  imap-message-append
	  imap-body-lines
	  imap-envelope-from
	  imap-send-command-1
	  imap-send-command
	  imap-wait-for-tag
	  imap-sentinel
	  imap-find-next-line
	  imap-arrival-filter
	  imap-parse-greeting
	  imap-parse-response
	  imap-parse-resp-text
	  imap-parse-resp-text-code
	  imap-parse-data-list
	  imap-parse-fetch
	  imap-parse-status
	  imap-parse-acl
	  imap-parse-flag-list
	  imap-parse-envelope
	  imap-parse-body-extension
	  imap-parse-body
	  )))

(provide 'imap)

;;; imap.el ends here
