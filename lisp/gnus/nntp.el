;;; nntp.el --- nntp access for Gnus

;; Copyright (C) 1987-1990, 1992-1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r)))
  ;; In Emacs 24, `open-protocol-stream' is an autoloaded alias for
  ;; `make-network-stream'.
  (unless (fboundp 'open-protocol-stream)
    (require 'proto-stream)))

(require 'nnheader)
(require 'nnoo)
(require 'gnus-util)
(require 'gnus)
(require 'gnus-group) ;; gnus-group-name-charset

(nnoo-declare nntp)

(eval-when-compile (require 'cl))

(autoload 'auth-source-search "auth-source")

(defgroup nntp nil
  "NNTP access for Gnus."
  :group 'gnus)

(defvoo nntp-address nil
  "Address of the physical nntp server.")

(defvoo nntp-port-number "nntp"
  "Port number on the physical nntp server.")

(defvoo nntp-server-opened-hook '(nntp-send-mode-reader)
  "*Hook used for sending commands to the server at startup.
The default value is `nntp-send-mode-reader', which makes an innd
server spawn an nnrpd server.")

(defvoo nntp-authinfo-function 'nntp-send-authinfo
  "Function used to send AUTHINFO to the server.
It is called with no parameters.")

(defvoo nntp-server-action-alist
    '(("nntpd 1\\.5\\.11t"
       (remove-hook 'nntp-server-opened-hook 'nntp-send-mode-reader))
      ("NNRP server Netscape"
       (setq nntp-server-list-active-group nil)))
  "Alist of regexps to match on server types and actions to be taken.
For instance, if you want Gnus to beep every time you connect
to innd, you could say something like:

\(setq nntp-server-action-alist
       '((\"innd\" (ding))))

You probably don't want to do that, though.")

(defvoo nntp-open-connection-function 'nntp-open-network-stream
  "Method for connecting to a remote system.
It should be a function, which is called with the output buffer
as its single argument, or one of the following special values:

- `nntp-open-network-stream' specifies a network connection,
  upgrading to a TLS connection via STARTTLS if possible.
- `nntp-open-plain-stream' specifies an unencrypted network
  connection (no STARTTLS upgrade is attempted).
- `nntp-open-ssl-stream' or `nntp-open-tls-stream' specify a TLS
  network connection.

Apart from the above special values, valid functions are as
follows; please refer to their respective doc string for more
information.
For direct connections:
- `nntp-open-netcat-stream'
- `nntp-open-telnet-stream'
For indirect connections:
- `nntp-open-via-rlogin-and-netcat'
- `nntp-open-via-rlogin-and-telnet'
- `nntp-open-via-telnet-and-telnet'")

(defvoo nntp-never-echoes-commands nil
  "*Non-nil means the nntp server never echoes commands.
It is reported that some nntps server doesn't echo commands.  So, you
may want to set this to non-nil in the method for such a server setting
`nntp-open-connection-function' to `nntp-open-ssl-stream' for example.
Note that the `nntp-open-connection-functions-never-echo-commands'
variable overrides the nil value of this variable.")

(defvoo nntp-open-connection-functions-never-echo-commands
    '(nntp-open-network-stream)
  "*List of functions that never echo commands.
Add or set a function which you set to `nntp-open-connection-function'
to this list if it does not echo commands.  Note that a non-nil value
of the `nntp-never-echoes-commands' variable overrides this variable.")

(defvoo nntp-pre-command nil
  "*Pre-command to use with the various nntp-open-via-* methods.
This is where you would put \"runsocks\" or stuff like that.")

(defvoo nntp-telnet-command "telnet"
  "*Telnet command used to connect to the nntp server.
This command is used by the methods `nntp-open-telnet-stream',
`nntp-open-via-rlogin-and-telnet' and `nntp-open-via-telnet-and-telnet'.")

(defvoo nntp-telnet-switches '("-8")
  "*Switches given to the telnet command `nntp-telnet-command'.")

(defvoo nntp-end-of-line "\r\n"
  "*String to use on the end of lines when talking to the NNTP server.
This is \"\\r\\n\" by default, but should be \"\\n\" when using an indirect
connection method (nntp-open-via-*).")

(defvoo nntp-via-rlogin-command "rsh"
  "*Rlogin command used to connect to an intermediate host.
This command is used by the methods `nntp-open-via-rlogin-and-telnet'
and `nntp-open-via-rlogin-and-netcat'.  The default is \"rsh\", but \"ssh\"
is a popular alternative.")

(defvoo nntp-via-rlogin-command-switches nil
  "*Switches given to the rlogin command `nntp-via-rlogin-command'.
If you use \"ssh\" for `nntp-via-rlogin-command', you may set this to
\(\"-C\") in order to compress all data connections, otherwise set this
to \(\"-t\" \"-e\" \"none\") or (\"-C\" \"-t\" \"-e\" \"none\") if the telnet
command requires a pseudo-tty allocation on an intermediate host.")

(defvoo nntp-via-telnet-command "telnet"
  "*Telnet command used to connect to an intermediate host.
This command is used by the `nntp-open-via-telnet-and-telnet' method.")

(defvoo nntp-via-telnet-switches '("-8")
  "*Switches given to the telnet command `nntp-via-telnet-command'.")

(defvoo nntp-netcat-command "nc"
  "*Netcat command used to connect to the nntp server.
This command is used by the `nntp-open-netcat-stream' and
`nntp-open-via-rlogin-and-netcat' methods.")

(defvoo nntp-netcat-switches nil
  "*Switches given to the netcat command `nntp-netcat-command'.")

(defvoo nntp-via-user-name nil
  "*User name to log in on an intermediate host with.
This variable is used by the various nntp-open-via-* methods.")

(defvoo nntp-via-user-password nil
  "*Password to use to log in on an intermediate host with.
This variable is used by the `nntp-open-via-telnet-and-telnet' method.")

(defvoo nntp-via-address nil
  "*Address of an intermediate host to connect to.
This variable is used by the various nntp-open-via-* methods.")

(defvoo nntp-via-envuser nil
  "*Whether both telnet client and server support the ENVIRON option.
If non-nil, there will be no prompt for a login name.")

(defvoo nntp-via-shell-prompt "bash\\|\$ *\r?$\\|> *\r?"
  "*Regular expression to match the shell prompt on an intermediate host.
This variable is used by the `nntp-open-via-telnet-and-telnet' method.")

(defvoo nntp-large-newsgroup 50
  "*The number of articles which indicates a large newsgroup.
If the number of articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvoo nntp-maximum-request 400
  "*The maximum number of the requests sent to the NNTP server at one time.
If Emacs hangs up while retrieving headers, set the variable to a
lower value.")

(defvoo nntp-nov-is-evil nil
  "*If non-nil, nntp will never attempt to use XOVER when talking to the server.")

(defvoo nntp-xover-commands '("XOVER" "XOVERVIEW")
  "*List of strings that are used as commands to fetch NOV lines from a server.
The strings are tried in turn until a positive response is gotten.  If
none of the commands are successful, nntp will just grab headers one
by one.")

(defvoo nntp-nov-gap 5
  "*Maximum allowed gap between two articles.
If the gap between two consecutive articles is bigger than this
variable, split the XOVER request into two requests.")

(defvoo nntp-xref-number-is-evil nil
  "*If non-nil, Gnus never trusts article numbers in the Xref header.
Some news servers, e.g., ones running Diablo, run multiple engines
having the same articles but article numbers are not kept synchronized
between them.  If you connect to such a server, set this to a non-nil
value, and Gnus never uses article numbers (that appear in the Xref
header and vary by which engine is chosen) to refer to articles.")

(defvoo nntp-prepare-server-hook nil
  "*Hook run before a server is opened.
If can be used to set up a server remotely, for instance.  Say you
have an account at the machine \"other.machine\".  This machine has
access to an NNTP server that you can't access locally.  You could
then use this hook to rsh to the remote machine and start a proxy NNTP
server there that you can connect to.  See also
`nntp-open-connection-function'")

(defvoo nntp-coding-system-for-read 'binary
  "*Coding system to read from NNTP.")

(defvoo nntp-coding-system-for-write 'binary
  "*Coding system to write to NNTP.")

;; Marks
(defvoo nntp-marks-is-evil nil
  "*If non-nil, Gnus will never generate and use marks file for nntp groups.
See `nnml-marks-is-evil' for more information.")

(defvoo nntp-marks-file-name ".marks")
(defvoo nntp-marks nil)
(defvar nntp-marks-modtime (gnus-make-hashtable))

(defcustom nntp-marks-directory
  (nnheader-concat gnus-directory "marks/")
  "*The directory where marks for nntp groups will be stored."
  :group 'nntp
  :type 'directory)

(defcustom nntp-authinfo-file "~/.authinfo"
  ".netrc-like file that holds nntp authinfo passwords."
  :group 'nntp
  :type
  '(choice file
	   (repeat :tag "Entries"
		   :menu-tag "Inline"
		   (list :format "%v"
			 :value ("" ("login" . "") ("password" . ""))
			 (string :tag "Host")
			 (checklist :inline t
				    (cons :format "%v"
					  (const :format "" "login")
					  (string :format "Login: %v"))
				    (cons :format "%v"
					  (const :format "" "password")
					  (string :format "Password: %v")))))))

(make-obsolete 'nntp-authinfo-file nil "Emacs 24.1")



(defvoo nntp-connection-timeout nil
  "*Number of seconds to wait before an nntp connection times out.
If this variable is nil, which is the default, no timers are set.
NOTE: This variable is never seen to work in Emacs 20 and XEmacs 21.")

(defvoo nntp-prepare-post-hook nil
  "*Hook run just before posting an article.  It is supposed to be used
to insert Cancel-Lock headers.")

(defvoo nntp-server-list-active-group 'try
  "If nil, then always use GROUP instead of LIST ACTIVE.
This is usually slower, but on misconfigured servers that don't
update their active files often, this can help.")

;;; Internal variables.

(defvoo nntp-retrieval-in-progress nil)
(defvar nntp-record-commands nil
  "*If non-nil, nntp will record all commands in the \"*nntp-log*\" buffer.")

(defvar nntp-have-messaged nil)

(defvar nntp-process-wait-for nil)
(defvar nntp-process-to-buffer nil)
(defvar nntp-process-callback nil)
(defvar nntp-process-decode nil)
(defvar nntp-process-start-point nil)
(defvar nntp-inside-change-function nil)
(defvoo nntp-last-command-time nil)
(defvoo nntp-last-command nil)
(defvoo nntp-authinfo-password nil)
(defvoo nntp-authinfo-user nil)
(defvoo nntp-authinfo-force nil)

(defvar nntp-connection-list nil)

(defvoo nntp-server-type nil)
(defvoo nntp-connection-alist nil)
(defvoo nntp-status-string "")
(defconst nntp-version "nntp 5.0")
(defvoo nntp-inhibit-erase nil)
(defvoo nntp-inhibit-output nil)

(defvoo nntp-server-xover 'try)

(defvar nntp-async-timer nil)
(defvar nntp-async-process-list nil)

(defvar nntp-authinfo-rejected nil
"A custom error condition used to report 'Authentication Rejected' errors.
Condition handlers that match just this condition ensure that the nntp
backend doesn't catch this error.")
(put 'nntp-authinfo-rejected 'error-conditions '(error nntp-authinfo-rejected))
(put 'nntp-authinfo-rejected 'error-message "Authorization Rejected")



;;; Internal functions.

(defsubst nntp-send-string (process string)
  "Send STRING to PROCESS."
  ;; We need to store the time to provide timeouts, and
  ;; to store the command so the we can replay the command
  ;; if the server gives us an AUTHINFO challenge.
  (setq nntp-last-command-time (current-time)
	nntp-last-command string)
  (when nntp-record-commands
    (nntp-record-command string))
  (process-send-string process (concat string nntp-end-of-line))
  (or (memq (process-status process) '(open run))
      (nntp-report "Server closed connection")))

(defun nntp-record-command (string)
  "Record the command STRING."
  (with-current-buffer (get-buffer-create "*nntp-log*")
    (goto-char (point-max))
    (insert (format-time-string "%Y%m%dT%H%M%S.%3N")
	    " " nntp-address " " string "\n")))

(defun nntp-report (&rest args)
  "Report an error from the nntp backend.  The first string in ARGS
can be a format string.  For some commands, the failed command may be
retried once before actually displaying the error report."

  (when nntp-record-commands
    (nntp-record-command "*** CALLED nntp-report ***"))

  (nnheader-report 'nntp args)

  (apply 'error args))

(defun nntp-report-1 (&rest args)
  "Throws out to nntp-with-open-group-error so that the connection may
be restored and the command retried."

  (when nntp-record-commands
    (nntp-record-command "*** CONNECTION LOST ***"))

  (throw 'nntp-with-open-group-error t))

(defmacro nntp-copy-to-buffer (buffer start end)
  "Copy string from unibyte current buffer to multibyte buffer."
  (if (featurep 'xemacs)
      `(copy-to-buffer ,buffer ,start ,end)
    `(let ((string (buffer-substring ,start ,end)))
       (with-current-buffer ,buffer
	 (erase-buffer)
	 (insert (if enable-multibyte-characters
		     (mm-string-to-multibyte string)
		   string))
	 (goto-char (point-min))
	 nil))))

(defsubst nntp-wait-for (process wait-for buffer &optional decode discard)
  "Wait for WAIT-FOR to arrive from PROCESS."

  (with-current-buffer (process-buffer process)
    (goto-char (point-min))

    (while (and (or (not (memq (char-after (point)) '(?2 ?3 ?4 ?5)))
		    (looking-at "48[02]"))
		(memq (process-status process) '(open run)))
      (cond ((looking-at "480")
	     (nntp-handle-authinfo process))
	    ((looking-at "482")
	     (nnheader-report 'nntp "%s"
			      (get 'nntp-authinfo-rejected 'error-message))
	     (signal 'nntp-authinfo-rejected nil))
	    ((looking-at "^.*\n")
	     (delete-region (point) (progn (forward-line 1) (point)))))
      (nntp-accept-process-output process)
      (goto-char (point-min)))
    (prog1
	(cond
	 ((looking-at "[45]")
	  (progn
	    (nntp-snarf-error-message)
	    nil))
	 ((not (memq (process-status process) '(open run)))
	  (nntp-report "Server closed connection"))
	 (t
	  (goto-char (point-max))
	  (let ((limit (point-min))
		response)
	    (while (not (re-search-backward wait-for limit t))
	      (nntp-accept-process-output process)
	      ;; We assume that whatever we wait for is less than 1000
	      ;; characters long.
	      (setq limit (max (- (point-max) 1000) (point-min)))
	      (goto-char (point-max)))
	    (setq response (match-string 0))
	    (with-current-buffer nntp-server-buffer
	      (setq nntp-process-response response)))
	  (nntp-decode-text (not decode))
	  (unless discard
	    (with-current-buffer buffer
	      (goto-char (point-max))
	      (nnheader-insert-buffer-substring (process-buffer process))
	      ;; Nix out "nntp reading...." message.
	      (when nntp-have-messaged
		(setq nntp-have-messaged nil)
		(nnheader-message 5 ""))))
	  t))
      (unless discard
	(erase-buffer)))))

(defun nntp-kill-buffer (buffer)
  (when (buffer-name buffer)
    (let ((process (get-buffer-process buffer)))
      (when process
	(delete-process process)))
    (kill-buffer buffer)
    (nnheader-init-server-buffer)))

(defun nntp-erase-buffer (buffer)
  "Erase contents of BUFFER."
  (with-current-buffer buffer
    (erase-buffer)))

(defsubst nntp-find-connection (buffer)
  "Find the connection delivering to BUFFER."
  (let ((alist nntp-connection-alist)
	(buffer (if (stringp buffer) (get-buffer buffer) buffer))
	process entry)
    (while (and alist (setq entry (pop alist)))
      (when (eq buffer (cadr entry))
	(setq process (car entry)
	      alist nil)))
    (when process
      (if (memq (process-status process) '(open run))
	  process
	(nntp-kill-buffer (process-buffer process))
	(setq nntp-connection-alist (delq entry nntp-connection-alist))
	nil))))

(defsubst nntp-find-connection-entry (buffer)
  "Return the entry for the connection to BUFFER."
  (assq (nntp-find-connection buffer) nntp-connection-alist))

(defun nntp-find-connection-buffer (buffer)
  "Return the process connection buffer tied to BUFFER."
  (let ((process (nntp-find-connection buffer)))
    (when process
      (process-buffer process))))

(defsubst nntp-retrieve-data (command address port buffer
				      &optional wait-for callback decode)
  "Use COMMAND to retrieve data into BUFFER from PORT on ADDRESS."
  (let ((process (or (nntp-find-connection buffer)
		     (nntp-open-connection buffer))))
    (if process
        (progn
          (unless (or nntp-inhibit-erase nnheader-callback-function)
	    (nntp-erase-buffer (process-buffer process)))
          (condition-case err
              (progn
                (when command
                  (nntp-send-string process command))
                (cond
                 ((eq callback 'ignore)
                  t)
                 ((and callback wait-for)
                  (nntp-async-wait process wait-for buffer decode callback)
                  t)
                 (wait-for
                  (nntp-wait-for process wait-for buffer decode))
                 (t t)))
	    (nntp-authinfo-rejected
	     (signal 'nntp-authinfo-rejected (cdr err)))
            (error
             (nnheader-report 'nntp "Couldn't open connection to %s: %s"
                              address err))
            (quit
             (message "Quit retrieving data from nntp")
             (signal 'quit nil)
             nil)))
      (nnheader-report 'nntp "Couldn't open connection to %s" address))))

(defsubst nntp-send-command (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (nntp-erase-buffer nntp-server-buffer))
  (let* ((command (mapconcat 'identity strings " "))
	 (process (nntp-find-connection nntp-server-buffer))
	 (buffer (and process (process-buffer process)))
	 (pos (and buffer (with-current-buffer buffer (point)))))
    (if process
	(prog1
	    (nntp-retrieve-data command
				nntp-address nntp-port-number
				nntp-server-buffer
				wait-for nnheader-callback-function)
	  ;; If nothing to wait for, still remove possibly echo'ed commands.
	  ;; We don't have echoes if `nntp-never-echoes-commands' is non-nil
	  ;; or the value of `nntp-open-connection-function' is in
	  ;; `nntp-open-connection-functions-never-echo-commands', so we
	  ;; skip this in that cases.
	  (unless (or wait-for
		      nntp-never-echoes-commands
		      (memq
		       nntp-open-connection-function
		       nntp-open-connection-functions-never-echo-commands))
	    (nntp-accept-response)
	    (with-current-buffer buffer
	      (goto-char pos)
	      (if (looking-at (regexp-quote command))
		  (delete-region pos (progn (forward-line 1)
					    (point-at-bol)))))))
      (nnheader-report 'nntp "Couldn't open connection to %s."
		       nntp-address))))

(defun nntp-send-command-nodelete (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (let* ((command (mapconcat 'identity strings " "))
	 (process (nntp-find-connection nntp-server-buffer))
	 (buffer (and process (process-buffer process)))
	 (pos (and buffer (with-current-buffer buffer (point)))))
    (if process
	(prog1
	    (nntp-retrieve-data command
				nntp-address nntp-port-number
				nntp-server-buffer
				wait-for nnheader-callback-function)
	  ;; If nothing to wait for, still remove possibly echo'ed commands
	  (unless wait-for
	    (nntp-accept-response)
	    (with-current-buffer buffer
	      (goto-char pos)
	      (if (looking-at (regexp-quote command))
		  (delete-region pos (progn (forward-line 1)
					    (point-at-bol)))))))
      (nnheader-report 'nntp "Couldn't open connection to %s."
		       nntp-address))))

(defun nntp-send-command-and-decode (wait-for &rest strings)
  "Send STRINGS to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (nntp-erase-buffer nntp-server-buffer))
  (let* ((command (mapconcat 'identity strings " "))
	 (process (nntp-find-connection nntp-server-buffer))
	 (buffer (and process (process-buffer process)))
	 (pos (and buffer (with-current-buffer buffer (point)))))
    (if process
	(prog1
	    (nntp-retrieve-data command
				nntp-address nntp-port-number
				nntp-server-buffer
				wait-for nnheader-callback-function t)
	  ;; If nothing to wait for, still remove possibly echo'ed commands
	  (unless wait-for
	    (nntp-accept-response)
	    (with-current-buffer buffer
	      (goto-char pos)
	      (if (looking-at (regexp-quote command))
		  (delete-region pos (progn (forward-line 1) (point-at-bol))))
	      )))
      (nnheader-report 'nntp "Couldn't open connection to %s."
		       nntp-address))))


(defun nntp-send-buffer (wait-for)
  "Send the current buffer to server and wait until WAIT-FOR returns."
  (when (and (not nnheader-callback-function)
	     (not nntp-inhibit-output))
    (nntp-erase-buffer
     (nntp-find-connection-buffer nntp-server-buffer)))
  (nntp-encode-text)
  ;; Make sure we did not forget to encode some of the content.
  (assert (save-excursion (goto-char (point-min))
                          (not (re-search-forward "[^\000-\377]" nil t))))
  (mm-disable-multibyte)
  (process-send-region (nntp-find-connection nntp-server-buffer)
                       (point-min) (point-max))
  (nntp-retrieve-data
   nil nntp-address nntp-port-number nntp-server-buffer
   wait-for nnheader-callback-function))



;;; Interface functions.

(nnoo-define-basics nntp)

(defsubst nntp-next-result-arrived-p ()
  (cond
   ;; A result that starts with a 2xx code is terminated by
   ;; a line with only a "." on it.
   ((eq (char-after) ?2)
    (if (re-search-forward "\n\\.\r?\n" nil t)
	(progn
	  ;; Some broken news servers add another dot at the end.
	  ;; Protect against inflooping there.
	  (while (looking-at "^\\.\r?\n")
	    (forward-line 1))
	  t)
      nil))
   ;; A result that starts with a 3xx or 4xx code is terminated
   ;; by a newline.
   ((looking-at "[34]")
    (if (search-forward "\n" nil t)
	t
      nil))
   ;; No result here.
   (t
    nil)))

(eval-when-compile
  (defvar nntp-with-open-group-internal nil)
  (defvar nntp-report-n nil))

(defun nntp-with-open-group-function (-group -server -connectionless -bodyfun)
  "Protect against servers that don't like clients that keep idle connections opens.
The problem being that these servers may either close a connection or
simply ignore any further requests on a connection.  Closed
connections are not detected until `accept-process-output' has updated
the `process-status'.  Dropped connections are not detected until the
connection timeouts (which may be several minutes) or
`nntp-connection-timeout' has expired.  When these occur
`nntp-with-open-group', opens a new connection then re-issues the NNTP
command whose response triggered the error."
  (letf ((nntp-report-n (symbol-function 'nntp-report))
         ((symbol-function 'nntp-report) (symbol-function 'nntp-report-1))
         (nntp-with-open-group-internal nil))
    (while (catch 'nntp-with-open-group-error
             ;; Open the connection to the server
             ;; NOTE: Existing connections are NOT tested.
             (nntp-possibly-change-group -group -server -connectionless)

             (let ((-timer
                    (and nntp-connection-timeout
                         (run-at-time
                          nntp-connection-timeout nil
                          (lambda ()
                            (let* ((-process (nntp-find-connection
                                             nntp-server-buffer))
                                   (-buffer  (and -process
                                                  (process-buffer -process))))
                              ;; When I an able to identify the
                              ;; connection to the server AND I've
                              ;; received NO response for
                              ;; nntp-connection-timeout seconds.
                              (when (and -buffer (eq 0 (buffer-size -buffer)))
                                ;; Close the connection.  Take no
                                ;; other action as the accept input
                                ;; code will handle the closed
                                ;; connection.
                                (nntp-kill-buffer -buffer))))))))
               (unwind-protect
                   (setq nntp-with-open-group-internal
                         (condition-case nil
                             (funcall -bodyfun)
                           (quit
                            (unless debug-on-quit
                              (nntp-close-server))
                            (signal 'quit nil))))
                 (when -timer
                   (nnheader-cancel-timer -timer)))
               nil))
      (setf (symbol-function 'nntp-report) nntp-report-n))
    nntp-with-open-group-internal))

(defmacro nntp-with-open-group (group server &optional connectionless &rest forms)
  "Protect against servers that don't like clients that keep idle connections opens.
The problem being that these servers may either close a connection or
simply ignore any further requests on a connection.  Closed
connections are not detected until `accept-process-output' has updated
the `process-status'.  Dropped connections are not detected until the
connection timeouts (which may be several minutes) or
`nntp-connection-timeout' has expired.  When these occur
`nntp-with-open-group', opens a new connection then re-issues the NNTP
command whose response triggered the error."
  (declare (indent 2) (debug (form form [&optional symbolp] def-body)))
  (when (and (listp connectionless)
	     (not (eq connectionless nil)))
    (setq forms (cons connectionless forms)
	  connectionless nil))
  `(nntp-with-open-group-function ,group ,server ,connectionless (lambda () ,@forms)))

(deffoo nntp-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve the headers of ARTICLES."
  (nntp-with-open-group
   group server
   (with-current-buffer (nntp-find-connection-buffer nntp-server-buffer)
     (erase-buffer)
     (if (and (not gnus-nov-is-evil)
              (not nntp-nov-is-evil)
              (nntp-retrieve-headers-with-xover articles fetch-old))
         ;; We successfully retrieved the headers via XOVER.
         'nov
       ;; XOVER didn't work, so we do it the hard, slow and inefficient
       ;; way.
       (let ((number (length articles))
             (articles articles)
             (count 0)
             (received 0)
             (last-point (point-min))
             (buf (nntp-find-connection-buffer nntp-server-buffer))
             (nntp-inhibit-erase t)
             article)
         ;; Send HEAD commands.
         (while (setq article (pop articles))
           (nntp-send-command
            nil
            "HEAD" (if (numberp article)
                       (int-to-string article)
                     ;; `articles' is either a list of article numbers
                     ;; or a list of article IDs.
                     article))
           (incf count)
           ;; Every 400 requests we have to read the stream in
           ;; order to avoid deadlocks.
           (when (or (null articles)    ;All requests have been sent.
                     (zerop (% count nntp-maximum-request)))
             (nntp-accept-response)
             (while (progn
                      (set-buffer buf)
                      (goto-char last-point)
                      ;; Count replies.
                      (while (nntp-next-result-arrived-p)
                        (setq last-point (point))
                        (incf received))
                      (< received count))
               ;; If number of headers is greater than 100, give
               ;;  informative messages.
               (and (numberp nntp-large-newsgroup)
                    (> number nntp-large-newsgroup)
                    (zerop (% received 20))
                    (nnheader-message 6 "NNTP: Receiving headers... %d%%"
                                      (/ (* received 100) number)))
               (nntp-accept-response))))
         (and (numberp nntp-large-newsgroup)
              (> number nntp-large-newsgroup)
              (nnheader-message 6 "NNTP: Receiving headers...done"))

         ;; Now all of replies are received.  Fold continuation lines.
         (nnheader-fold-continuation-lines)
         ;; Remove all "\r"'s.
         (nnheader-strip-cr)
	 (nntp-copy-to-buffer nntp-server-buffer (point-min) (point-max))
         'headers)))))

(deffoo nntp-retrieve-group-data-early (server infos)
  "Retrieve group info on INFOS."
  (nntp-with-open-group nil server
    (let ((buffer (nntp-find-connection-buffer nntp-server-buffer)))
      (unless infos
	(with-current-buffer buffer
	  (setq nntp-retrieval-in-progress nil)))
      (when (and buffer
		 infos
		 (with-current-buffer buffer
		   (not nntp-retrieval-in-progress)))
	;; The first time this is run, this variable is `try'.  So we
	;; try.
	(when (eq nntp-server-list-active-group 'try)
	  (nntp-try-list-active
	   (gnus-group-real-name (gnus-info-group (car infos)))))
	(with-current-buffer buffer
	  (erase-buffer)
	  ;; Mark this buffer as "in use" in case we try to issue two
	  ;; retrievals from the same server.  This shouldn't happen,
	  ;; so this is mostly a sanity check.
	  (setq nntp-retrieval-in-progress t)
	  (let ((nntp-inhibit-erase t)
		(command (if nntp-server-list-active-group
			     "LIST ACTIVE" "GROUP")))
	    (dolist (info infos)
	      (nntp-send-command
	       nil command (gnus-group-real-name (gnus-info-group info)))))
	  (length infos))))))

(deffoo nntp-finish-retrieve-group-infos (server infos count)
  (nntp-with-open-group nil server
    (let ((buf (nntp-find-connection-buffer nntp-server-buffer))
	  (method (gnus-find-method-for-group
		   (gnus-info-group (car infos))
		   (car infos)))
	  (received 0)
	  (last-point 1))
      (with-current-buffer buf
	(setq nntp-retrieval-in-progress nil))
      (when (and buf
		 count)
	(with-current-buffer buf
	  (while (and (gnus-buffer-live-p buf)
		      (progn
			(goto-char last-point)
			;; Count replies.
			(while (re-search-forward
				(if nntp-server-list-active-group
				    "^[.]"
				  "^[0-9]")
				nil t)
			  (incf received))
			(setq last-point (point))
			(< received count)))
	    (nntp-accept-response))
	  ;; We now have all the entries.  Remove CRs.
	  (nnheader-strip-cr)
	  (if (not nntp-server-list-active-group)
	      (progn
		(nntp-copy-to-buffer nntp-server-buffer
				     (point-min) (point-max))
		(gnus-groups-to-gnus-format method gnus-active-hashtb t))
	    ;; We have read active entries, so we just delete the
	    ;; superfluous gunk.
	    (goto-char (point-min))
	    (while (re-search-forward "^[.2-5]" nil t)
	      (delete-region (match-beginning 0)
			     (progn (forward-line 1) (point))))
	    (nntp-copy-to-buffer nntp-server-buffer (point-min) (point-max))
	    (with-current-buffer nntp-server-buffer
	      (gnus-active-to-gnus-format
	       ;; Kludge to use the extended method name if you have
	       ;; an extended one.
	       (if (consp (gnus-info-method (car infos)))
		   (gnus-info-method (car infos))
		 method)
	       gnus-active-hashtb nil t))))))))

(deffoo nntp-retrieve-groups (groups &optional server)
  "Retrieve group info on GROUPS."
  (nntp-with-open-group
   nil server
   (when (and (nntp-find-connection-buffer nntp-server-buffer)
	      (with-current-buffer
		  (nntp-find-connection-buffer nntp-server-buffer)
		(if (not nntp-retrieval-in-progress)
		    t
		  (message "Warning: Refusing to do retrieval from %s because a retrieval is already happening"
			   server)
		  nil)))
     (catch 'done
       (save-excursion
         ;; Erase nntp-server-buffer before nntp-inhibit-erase.
	 (nntp-erase-buffer nntp-server-buffer)
         (set-buffer (nntp-find-connection-buffer nntp-server-buffer))
         ;; The first time this is run, this variable is `try'.  So we
         ;; try.
         (when (eq nntp-server-list-active-group 'try)
           (nntp-try-list-active (car groups)))
         (erase-buffer)
         (let ((count 0)
               (groups groups)
               (received 0)
               (last-point (point-min))
               (nntp-inhibit-erase t)
               (buf (nntp-find-connection-buffer nntp-server-buffer))
               (command (if nntp-server-list-active-group
                            "LIST ACTIVE" "GROUP")))
           (while groups
             ;; Timeout may have killed the buffer.
             (unless (gnus-buffer-live-p buf)
               (nnheader-report 'nntp "Connection to %s is closed." server)
               (throw 'done nil))
             ;; Send the command to the server.
             (nntp-send-command nil command (pop groups))
             (incf count)
             ;; Every 400 requests we have to read the stream in
             ;; order to avoid deadlocks.
             (when (or (null groups)    ;All requests have been sent.
                       (zerop (% count nntp-maximum-request)))
               (nntp-accept-response)
               (while (and (gnus-buffer-live-p buf)
                           (progn
                             ;; Search `blue moon' in this file for the
                             ;; reason why set-buffer here.
                             (set-buffer buf)
                             (goto-char last-point)
                             ;; Count replies.
                             (while (re-search-forward "^[0-9]" nil t)
                               (incf received))
                             (setq last-point (point))
                             (< received count)))
                 (nntp-accept-response))))

           ;; Wait for the reply from the final command.
           (unless (gnus-buffer-live-p buf)
             (nnheader-report 'nntp "Connection to %s is closed." server)
             (throw 'done nil))
           (set-buffer buf)
           (goto-char (point-max))
           (re-search-backward "^[0-9]" nil t)
           (when (looking-at "^[23]")
             (while (and (gnus-buffer-live-p buf)
                         (progn
                           (set-buffer buf)
                           (goto-char (point-max))
                           (if (not nntp-server-list-active-group)
                               (not (re-search-backward "\r?\n"
							(- (point) 3) t))
                             (not (re-search-backward "^\\.\r?\n"
                                                      (- (point) 4) t)))))
               (nntp-accept-response)))

           ;; Now all replies are received.  We remove CRs.
           (unless (gnus-buffer-live-p buf)
             (nnheader-report 'nntp "Connection to %s is closed." server)
             (throw 'done nil))
           (set-buffer buf)
           (goto-char (point-min))
           (while (search-forward "\r" nil t)
             (replace-match "" t t))

           (if (not nntp-server-list-active-group)
               (progn
		 (nntp-copy-to-buffer nntp-server-buffer
				      (point-min) (point-max))
                 'group)
             ;; We have read active entries, so we just delete the
             ;; superfluous gunk.
             (goto-char (point-min))
             (while (re-search-forward "^[.2-5]" nil t)
               (delete-region (match-beginning 0)
                              (progn (forward-line 1) (point))))
	     (nntp-copy-to-buffer nntp-server-buffer (point-min) (point-max))
             'active)))))))

(deffoo nntp-retrieve-articles (articles &optional group server)
  (nntp-with-open-group
    group server
   (save-excursion
     (let ((number (length articles))
           (articles articles)
           (count 0)
           (received 0)
           (last-point (point-min))
           (buf (nntp-find-connection-buffer nntp-server-buffer))
           (nntp-inhibit-erase t)
           (map (apply 'vector articles))
           (point 1)
           article)
       (set-buffer buf)
       (erase-buffer)
       ;; Send ARTICLE command.
       (while (setq article (pop articles))
         (nntp-send-command
          nil
          "ARTICLE" (if (numberp article)
                        (int-to-string article)
                      ;; `articles' is either a list of article numbers
                      ;; or a list of article IDs.
                      article))
         (incf count)
         ;; Every 400 requests we have to read the stream in
         ;; order to avoid deadlocks.
         (when (or (null articles)	;All requests have been sent.
                   (zerop (% count nntp-maximum-request)))
           (nntp-accept-response)
           (while (progn
                    (set-buffer buf)
                    (goto-char last-point)
                    ;; Count replies.
                    (while (nntp-next-result-arrived-p)
                      (aset map received (cons (aref map received) (point)))
                      (setq last-point (point))
                      (incf received))
                    (< received count))
             ;; If number of headers is greater than 100, give
             ;;  informative messages.
             (and (numberp nntp-large-newsgroup)
                  (> number nntp-large-newsgroup)
                  (zerop (% received 20))
                  (nnheader-message 6 "NNTP: Receiving articles... %d%%"
                                    (/ (* received 100) number)))
             (nntp-accept-response))))
       (and (numberp nntp-large-newsgroup)
            (> number nntp-large-newsgroup)
            (nnheader-message 6 "NNTP: Receiving articles...done"))

       ;; Now we have all the responses.  We go through the results,
       ;; wash it and copy it over to the server buffer.
       (set-buffer nntp-server-buffer)
       (erase-buffer)
       (setq last-point (point-min))
       (mapcar
        (lambda (entry)
          (narrow-to-region
           (setq point (goto-char (point-max)))
           (progn
	     (nnheader-insert-buffer-substring buf last-point (cdr entry))
             (point-max)))
          (setq last-point (cdr entry))
          (nntp-decode-text)
          (widen)
          (cons (car entry) point))
        map)))))

(defun nntp-try-list-active (group)
  (nntp-list-active-group group)
  (with-current-buffer nntp-server-buffer
    (goto-char (point-min))
    (cond ((or (eobp)
	       (looking-at "5[0-9]+"))
	   (setq nntp-server-list-active-group nil))
	  (t
	   (setq nntp-server-list-active-group t)))))

(deffoo nntp-list-active-group (group &optional server)
  "Return the active info on GROUP (which can be a regexp)."
  (nntp-with-open-group
   nil server
   (nntp-send-command "^\\.*\r?\n" "LIST ACTIVE" group)))

(deffoo nntp-request-group-articles (group &optional server)
  "Return the list of existing articles in GROUP."
  (nntp-with-open-group
   nil server
   (nntp-send-command "^\\.*\r?\n" "LISTGROUP" group)))

(deffoo nntp-request-article (article &optional group server buffer command)
  (nntp-with-open-group
      group server
    (when (nntp-send-command-and-decode
           "\r?\n\\.\r?\n" "ARTICLE"
           (if (numberp article) (int-to-string article) article))
      (when (and buffer
		 (not (equal buffer nntp-server-buffer)))
	(with-current-buffer nntp-server-buffer
	  (copy-to-buffer buffer (point-min) (point-max))))
      (nntp-find-group-and-number group))))

(deffoo nntp-request-head (article &optional group server)
  (nntp-with-open-group
   group server
   (when (nntp-send-command
          "\r?\n\\.\r?\n" "HEAD"
          (if (numberp article) (int-to-string article) article))
     (prog1
         (nntp-find-group-and-number group)
       (nntp-decode-text)))))

(deffoo nntp-request-body (article &optional group server)
  (nntp-with-open-group
   group server
   (nntp-send-command-and-decode
    "\r?\n\\.\r?\n" "BODY"
    (if (numberp article) (int-to-string article) article))))

(deffoo nntp-request-group (group &optional server dont-check info)
  (nntp-with-open-group
    nil server
    (when (nntp-send-command "^[245].*\n" "GROUP" group)
      (let ((entry (nntp-find-connection-entry nntp-server-buffer)))
        (setcar (cddr entry) group)))))

(deffoo nntp-close-group (group &optional server)
  t)

(deffoo nntp-server-opened (&optional server)
  "Say whether a connection to SERVER has been opened."
  (and (nnoo-current-server-p 'nntp server)
       nntp-server-buffer
       (gnus-buffer-live-p nntp-server-buffer)
       (nntp-find-connection nntp-server-buffer)))

(deffoo nntp-open-server (server &optional defs connectionless)
  (nnheader-init-server-buffer)
  (if (nntp-server-opened server)
      t
    (when (or (stringp (car defs))
	      (numberp (car defs)))
      (setq defs (cons (list 'nntp-port-number (car defs)) (cdr defs))))
    (unless (assq 'nntp-address defs)
      (setq defs (append defs (list (list 'nntp-address server)))))
    (nnoo-change-server 'nntp server defs)
    (if connectionless
	t
      (or (nntp-find-connection nntp-server-buffer)
	  (nntp-open-connection nntp-server-buffer)))))

(deffoo nntp-close-server (&optional server)
  (nntp-possibly-change-group nil server t)
  (let ((process (nntp-find-connection nntp-server-buffer)))
    (while process
      (when (memq (process-status process) '(open run))
	(ignore-errors
	  (nntp-send-string process "QUIT")
	  (unless (eq nntp-open-connection-function 'nntp-open-network-stream)
	    ;; Ok, this is evil, but when using telnet and stuff
	    ;; as the connection method, it's important that the
	    ;; QUIT command actually is sent out before we kill
	    ;; the process.
	    (sleep-for 1))))
      (nntp-kill-buffer (process-buffer process))
      (setq process (car (pop nntp-connection-alist))))
    (nnoo-close-server 'nntp)))

(deffoo nntp-request-close ()
  (let (process)
    (while (setq process (pop nntp-connection-list))
      (when (memq (process-status process) '(open run))
	(ignore-errors
	  (nntp-send-string process "QUIT")
	  (unless (eq nntp-open-connection-function 'nntp-open-network-stream)
	    ;; Ok, this is evil, but when using telnet and stuff
	    ;; as the connection method, it's important that the
	    ;; QUIT command actually is sent out before we kill
	    ;; the process.
	    (sleep-for 1))))
      (nntp-kill-buffer (process-buffer process)))))

(deffoo nntp-request-list (&optional server)
  (nntp-with-open-group
   nil server
   (nntp-send-command-and-decode "\r?\n\\.\r?\n" "LIST")))

(deffoo nntp-request-list-newsgroups (&optional server)
  (nntp-with-open-group
   nil server
   (nntp-send-command "\r?\n\\.\r?\n" "LIST NEWSGROUPS")))

(deffoo nntp-request-newgroups (date &optional server)
  (nntp-with-open-group
   nil server
   (with-current-buffer nntp-server-buffer
     (let* ((time (date-to-time date))
            (ls (- (cadr time) (nth 8 (decode-time time)))))
       (cond ((< ls 0)
              (setcar time (1- (car time)))
              (setcar (cdr time) (+ ls 65536)))
             ((>= ls 65536)
              (setcar time (1+ (car time)))
              (setcar (cdr time) (- ls 65536)))
             (t
              (setcar (cdr time) ls)))
       (prog1
           (nntp-send-command
            "^\\.\r?\n" "NEWGROUPS"
            (format-time-string "%y%m%d %H%M%S" time)
            "GMT")
         (nntp-decode-text))))))

(deffoo nntp-request-post (&optional server)
  (nntp-with-open-group
   nil server
   (when (nntp-send-command "^[23].*\r?\n" "POST")
     (let ((response (with-current-buffer nntp-server-buffer
                       nntp-process-response))
           server-id)
       (when (and response
                  (string-match "^[23].*\\(<[^\t\n @<>]+@[^\t\n @<>]+>\\)"
                                response))
         (setq server-id (match-string 1 response))
         (narrow-to-region (goto-char (point-min))
                           (if (search-forward "\n\n" nil t)
                               (1- (point))
                             (point-max)))
         (unless (mail-fetch-field "Message-ID")
           (goto-char (point-min))
           (insert "Message-ID: " server-id "\n"))
         (widen))
       (run-hooks 'nntp-prepare-post-hook)
       (nntp-send-buffer "^[23].*\n")))))

(deffoo nntp-request-type (group article)
  'news)

(deffoo nntp-asynchronous-p ()
  t)

(deffoo nntp-request-set-mark (group actions &optional server)
  (when (and (not nntp-marks-is-evil)
	     nntp-marks-file-name)
    (nntp-possibly-create-directory group server)
    (nntp-open-marks group server)
    (setq nntp-marks (nnheader-update-marks-actions nntp-marks actions))
    (nntp-save-marks group server))
  nil)

(deffoo nntp-request-marks (group info &optional server)
  (when (and (not nntp-marks-is-evil)
	     nntp-marks-file-name)
    (nntp-possibly-create-directory group server)
    (when (nntp-marks-changed-p group server)
      (nnheader-message 8 "Updating marks for %s..." group)
      (nntp-open-marks group server)
      ;; Update info using `nntp-marks'.
      (mapc (lambda (pred)
	      (unless (memq (cdr pred) gnus-article-unpropagated-mark-lists)
		(gnus-info-set-marks
		 info
		 (gnus-update-alist-soft
		  (cdr pred)
		  (cdr (assq (cdr pred) nntp-marks))
		  (gnus-info-marks info))
		 t)))
	    gnus-article-mark-lists)
      (let ((seen (cdr (assq 'read nntp-marks))))
	(gnus-info-set-read info
			    (if (and (integerp (car seen))
				     (null (cdr seen)))
				(list (cons (car seen) (car seen)))
			      seen)))
      (nnheader-message 8 "Updating marks for %s...done" group)))
  nil)



;;; Hooky functions.

(defun nntp-send-mode-reader ()
  "Send the MODE READER command to the nntp server.
This function is supposed to be called from `nntp-server-opened-hook'.
It will make innd servers spawn an nnrpd process to allow actual article
reading."
  (nntp-send-command "^.*\n" "MODE READER"))

(declare-function netrc-parse "netrc" (&optional file))
(declare-function netrc-machine "netrc"
		  (list machine &optional port defaultport))
(declare-function netrc-get "netrc" (alist type))

(defun nntp-send-authinfo (&optional send-if-force)
  "Send the AUTHINFO to the nntp server.
It will look in the \"~/.authinfo\" file for matching entries.  If
nothing suitable is found there, it will prompt for a user name
and a password.

If SEND-IF-FORCE, only send authinfo to the server if the
.authinfo file has the FORCE token."
  (require 'netrc)
  (let* ((list (netrc-parse nntp-authinfo-file))
	 (alist (netrc-machine list nntp-address "nntp"))
         (auth-info
          (nth 0 (auth-source-search
		  :max 1
		  :host (list nntp-address (nnoo-current-server 'nntp))
		  :port `("119" "nntp" ,(format "%s" nntp-port-number)
			  "563" "nntps" "snews"))))
         (auth-user (plist-get auth-info :user))
         (auth-force (plist-get auth-info :force))
         (auth-passwd (plist-get auth-info :secret))
         (auth-passwd (if (functionp auth-passwd)
                          (funcall auth-passwd)
                        auth-passwd))
	 (force (or (netrc-get alist "force")
                    nntp-authinfo-force
                    auth-force))
	 (user (or
		;; this is preferred to netrc-*
		auth-user
		(netrc-get alist "login")
		nntp-authinfo-user))
	 (passwd (or
		  ;; this is preferred to netrc-*
		  auth-passwd
		  (netrc-get alist "password"))))
    (when (or (not send-if-force)
	      force)
      (unless user
	(setq user (read-string (format "NNTP (%s) user name: " nntp-address))
	      nntp-authinfo-user user))
      (unless (member user '(nil ""))
	(nntp-send-command "^3.*\r?\n" "AUTHINFO USER" user)
	(when t				;???Should check if AUTHINFO succeeded
	  (nntp-send-command
	   "^2.*\r?\n" "AUTHINFO PASS"
	   (or passwd
	       nntp-authinfo-password
	       (setq nntp-authinfo-password
		     (read-passwd (format "NNTP (%s@%s) password: "
					  user nntp-address))))))))))

(defun nntp-send-nosy-authinfo ()
  "Send the AUTHINFO to the nntp server."
  (let ((user (read-string (format "NNTP (%s) user name: " nntp-address))))
    (unless (member user '(nil ""))
      (nntp-send-command "^3.*\r?\n" "AUTHINFO USER" user)
      (when t				;???Should check if AUTHINFO succeeded
	(nntp-send-command "^2.*\r?\n" "AUTHINFO PASS"
			   (read-passwd (format "NNTP (%s@%s) password: "
						user nntp-address)))))))

(defun nntp-send-authinfo-from-file ()
  "Send the AUTHINFO to the nntp server.

The authinfo login name is taken from the user's login name and the
password contained in '~/.nntp-authinfo'."
  (when (file-exists-p "~/.nntp-authinfo")
    (with-temp-buffer
      (insert-file-contents "~/.nntp-authinfo")
      (goto-char (point-min))
      (nntp-send-command "^3.*\r?\n" "AUTHINFO USER" (user-login-name))
      (nntp-send-command
       "^2.*\r?\n" "AUTHINFO PASS"
       (buffer-substring (point) (point-at-eol))))))

;;; Internal functions.

(defun nntp-handle-authinfo (process)
  "Take care of an authinfo response from the server."
  (let ((last nntp-last-command))
    (funcall nntp-authinfo-function)
    ;; We have to re-send the function that was interrupted by
    ;; the authinfo request.
    (nntp-erase-buffer nntp-server-buffer)
    (nntp-send-string process last)))

(defun nntp-make-process-buffer (buffer)
  "Create a new, fresh buffer usable for nntp process connections."
  (with-current-buffer
      (generate-new-buffer
       (format " *server %s %s %s*"
               nntp-address nntp-port-number
               (gnus-buffer-exists-p buffer)))
    (mm-disable-multibyte)
    (set (make-local-variable 'after-change-functions) nil)
    (set (make-local-variable 'nntp-process-wait-for) nil)
    (set (make-local-variable 'nntp-process-callback) nil)
    (set (make-local-variable 'nntp-process-to-buffer) nil)
    (set (make-local-variable 'nntp-process-start-point) nil)
    (set (make-local-variable 'nntp-process-decode) nil)
    (set (make-local-variable 'nntp-retrieval-in-progress) nil)
    (current-buffer)))

(defun nntp-open-connection (buffer)
  "Open a connection to PORT on ADDRESS delivering output to BUFFER."
  (run-hooks 'nntp-prepare-server-hook)
  (let* ((pbuffer (nntp-make-process-buffer buffer))
	 (timer
	  (and nntp-connection-timeout
	       (run-at-time
		nntp-connection-timeout nil
		`(lambda ()
		   (nntp-kill-buffer ,pbuffer)))))
	 (process
	  (condition-case err
	      (let ((coding-system-for-read nntp-coding-system-for-read)
		    (coding-system-for-write nntp-coding-system-for-write)
		    (map '((nntp-open-network-stream network)
			   (network-only plain) ; compat
			   (nntp-open-plain-stream plain)
			   (nntp-open-ssl-stream tls)
			   (nntp-open-tls-stream tls))))
		(if (assoc nntp-open-connection-function map)
		    (open-protocol-stream
		     "nntpd" pbuffer nntp-address nntp-port-number
		     :type (cadr (assoc nntp-open-connection-function map))
		     :end-of-command "^\\([2345]\\|[.]\\).*\n"
		     :capability-command "CAPABILITIES\r\n"
		     :success "^3"
		     :starttls-function
		     (lambda (capabilities)
		       (if (not (string-match "STARTTLS" capabilities))
			   nil
			 "STARTTLS\r\n")))
		  (funcall nntp-open-connection-function pbuffer)))
	    (error
	     (nnheader-report 'nntp ">>> %s" err))
	    (quit
	     (message "Quit opening connection to %s" nntp-address)
	     (nntp-kill-buffer pbuffer)
	     (signal 'quit nil)
	     nil))))
    (when timer
      (nnheader-cancel-timer timer))
    (when (and process
	       (not (memq (process-status process) '(open run))))
      (with-current-buffer pbuffer
	(goto-char (point-min))
	(nnheader-report 'nntp "Error when connecting: %s"
			 (buffer-substring (point) (line-end-position))))
      (setq process nil))
    (unless process
      (nntp-kill-buffer pbuffer))
    (when (and (buffer-name pbuffer)
	       process)
      (when (and (fboundp 'set-network-process-option) ;; Unavailable in XEmacs.
		 (fboundp 'process-type) ;; Emacs 22 doesn't provide it.
                 (eq (process-type process) 'network))
        ;; Use TCP-keepalive so that connections that pass through a NAT router
        ;; don't hang when left idle.
        (set-network-process-option process :keepalive t))
      (gnus-set-process-query-on-exit-flag process nil)
      (if (and (nntp-wait-for process "^2.*\n" buffer nil t)
	       (memq (process-status process) '(open run)))
	  (prog1
	      (caar (push (list process buffer nil) nntp-connection-alist))
	    (push process nntp-connection-list)
	    (with-current-buffer pbuffer
	      (nntp-read-server-type)
	      (erase-buffer)
	      (set-buffer nntp-server-buffer)
	      (let ((nnheader-callback-function nil))
		(run-hooks 'nntp-server-opened-hook)
		(nntp-send-authinfo t))))
	(nntp-kill-buffer (process-buffer process))
	nil))))

(defun nntp-read-server-type ()
  "Find out what the name of the server we have connected to is."
  ;; Wait for the status string to arrive.
  (setq nntp-server-type (buffer-string))
  (let ((case-fold-search t))
    ;; Run server-specific commands.
    (dolist (entry nntp-server-action-alist)
      (when (string-match (car entry) nntp-server-type)
	(if (and (listp (cadr entry))
		 (not (eq 'lambda (caadr entry))))
	    (eval (cadr entry))
	  (funcall (cadr entry)))))))

(defun nntp-async-wait (process wait-for buffer decode callback)
  (with-current-buffer (process-buffer process)
    (unless nntp-inside-change-function
      (erase-buffer))
    (setq nntp-process-wait-for wait-for
	  nntp-process-to-buffer buffer
	  nntp-process-decode decode
	  nntp-process-callback callback
	  nntp-process-start-point (point-max))
    (setq after-change-functions '(nntp-after-change-function))))

(defun nntp-async-timer-handler ()
  (mapcar
   (lambda (proc)
     (if (memq (process-status proc) '(open run))
	 (nntp-async-trigger proc)
       (nntp-async-stop proc)))
   nntp-async-process-list))

(defun nntp-async-stop (proc)
  (setq nntp-async-process-list (delq proc nntp-async-process-list))
  (when (and nntp-async-timer (not nntp-async-process-list))
    (nnheader-cancel-timer nntp-async-timer)
    (setq nntp-async-timer nil)))

(defun nntp-after-change-function (beg end len)
  (unwind-protect
      ;; we only care about insertions at eob
      (when (and (eq 0 len) (eq (point-max) end))
	(save-match-data
	  (let ((proc (get-buffer-process (current-buffer))))
	    (when proc
	      (nntp-async-trigger proc)))))
    ;; any throw from after-change-functions will leave it
    ;; set to nil.  so we reset it here, if necessary.
    (when quit-flag
      (setq after-change-functions '(nntp-after-change-function)))))

(defun nntp-async-trigger (process)
  (with-current-buffer (process-buffer process)
    (when nntp-process-callback
      ;; do we have an error message?
      (goto-char nntp-process-start-point)
      (if (memq (following-char) '(?4 ?5))
	  ;; wants credentials?
	  (if (looking-at "480")
	      (nntp-handle-authinfo process)
	    ;; report error message.
	    (nntp-snarf-error-message)
	    (nntp-do-callback nil))

	;; got what we expect?
	(goto-char (point-max))
	(when (re-search-backward
	       nntp-process-wait-for nntp-process-start-point t)
	  (let ((response (match-string 0)))
	    (with-current-buffer nntp-server-buffer
	      (setq nntp-process-response response)))
	  (nntp-async-stop process)
	  ;; convert it.
	  (when (gnus-buffer-exists-p nntp-process-to-buffer)
	    (let ((buf (current-buffer))
		  (start nntp-process-start-point)
		  (decode nntp-process-decode))
	      (with-current-buffer nntp-process-to-buffer
		(goto-char (point-max))
		(save-restriction
		  (narrow-to-region (point) (point))
		  (nnheader-insert-buffer-substring buf start)
		  (when decode
		    (nntp-decode-text))))))
	  ;; report it.
	  (goto-char (point-max))
	  (nntp-do-callback
	   (buffer-name (get-buffer nntp-process-to-buffer))))))))

(defun nntp-do-callback (arg)
  (let ((callback nntp-process-callback)
	(nntp-inside-change-function t))
    (setq nntp-process-callback nil)
    (funcall callback arg)))

(defun nntp-snarf-error-message ()
  "Save the error message in the current buffer."
  (let ((message (buffer-string)))
    (while (string-match "[\r\n]+" message)
      (setq message (replace-match " " t t message)))
    (nnheader-report 'nntp "%s" message)
    message))

(defun nntp-accept-process-output (process)
  "Wait for output from PROCESS and message some dots."
  (with-current-buffer (or (nntp-find-connection-buffer nntp-server-buffer)
                           nntp-server-buffer)
    (let ((len (/ (buffer-size) 1024))
	  message-log-max)
      (unless (< len 10)
	(setq nntp-have-messaged t)
	(nnheader-message 7 "nntp read: %dk" len)))
    (prog1
	(nnheader-accept-process-output process)
      ;; accept-process-output may update status of process to indicate
      ;; that the server has closed the connection.  This MUST be
      ;; handled here as the buffer restored by the save-excursion may
      ;; be the process's former output buffer (i.e. now killed)
      (or (and process
	       (memq (process-status process) '(open run)))
          (nntp-report "Server closed connection")))))

(defun nntp-accept-response ()
  "Wait for output from the process that outputs to BUFFER."
  (nntp-accept-process-output (nntp-find-connection nntp-server-buffer)))

(defun nntp-possibly-change-group (group server &optional connectionless)
  (let ((nnheader-callback-function nil))
    (when server
      (or (nntp-server-opened server)
	  (nntp-open-server server nil connectionless)))

    (unless connectionless
      (or (nntp-find-connection nntp-server-buffer)
	  (nntp-open-connection nntp-server-buffer))))

  (when group
    (let ((entry (nntp-find-connection-entry nntp-server-buffer)))
      (cond ((not entry)
             (nntp-report "Server closed connection"))
            ((not (equal group (caddr entry)))
             (with-current-buffer (process-buffer (car entry))
               (erase-buffer)
               (nntp-send-command "^[245].*\n" "GROUP" group)
               (setcar (cddr entry) group)
               (erase-buffer)
	       (nntp-erase-buffer nntp-server-buffer)))))))

(defun nntp-decode-text (&optional cr-only)
  "Decode the text in the current buffer."
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (delete-char -1))
  (unless cr-only
    ;; Remove trailing ".\n" end-of-transfer marker.
    (goto-char (point-max))
    (forward-line -1)
    (when (looking-at ".\n")
      (delete-char 2))
    ;; Delete status line.
    (goto-char (point-min))
    (while (looking-at "[1-5][0-9][0-9] .*\n")
      ;; For some unknown reason, there is more than one status line.
      (delete-region (point) (progn (forward-line 1) (point))))
    ;; Remove "." -> ".." encoding.
    (while (search-forward "\n.." nil t)
      (delete-char -1))))

(defun nntp-encode-text ()
  "Encode the text in the current buffer."
  (save-excursion
    ;; Replace "." at beginning of line with "..".
    (goto-char (point-min))
    (while (re-search-forward "^\\." nil t)
      (insert "."))
    (goto-char (point-max))
    ;; Insert newline at the end of the buffer.
    (unless (bolp)
      (insert "\n"))
    ;; Insert `.' at end of buffer (end of text mark).
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (delete-char 1)
      (insert nntp-end-of-line))))

(defun nntp-retrieve-headers-with-xover (articles &optional fetch-old)
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (cond

   ;; This server does not talk NOV.
   ((not nntp-server-xover)
    nil)

   ;; We don't care about gaps.
   ((or (not nntp-nov-gap)
	fetch-old)
    (nntp-send-xover-command
     (if fetch-old
	 (if (numberp fetch-old)
	     (max 1 (- (car articles) fetch-old))
	   1)
       (car articles))
     (car (last articles)) 'wait)

    (goto-char (point-min))
    (when (looking-at "[1-5][0-9][0-9] .*\n")
      (delete-region (point) (progn (forward-line 1) (point))))
    (while (search-forward "\r" nil t)
      (replace-match "" t t))
    (goto-char (point-max))
    (forward-line -1)
    (when (looking-at "\\.")
      (delete-region (point) (progn (forward-line 1) (point)))))

   ;; We do it the hard way.  For each gap, an XOVER command is sent
   ;; to the server.  We do not wait for a reply from the server, we
   ;; just send them off as fast as we can.  That means that we have
   ;; to count the number of responses we get back to find out when we
   ;; have gotten all we asked for.
   ((numberp nntp-nov-gap)
    (let ((count 0)
	  (received 0)
	  last-point
	  in-process-buffer-p
	  (buf nntp-server-buffer)
	  (process-buffer (nntp-find-connection-buffer nntp-server-buffer))
	  first last status)
      ;; We have to check `nntp-server-xover'.  If it gets set to nil,
      ;; that means that the server does not understand XOVER, but we
      ;; won't know that until we try.
      (while (and nntp-server-xover articles)
	(setq first (car articles))
	;; Search forward until we find a gap, or until we run out of
	;; articles.
	(while (and (cdr articles)
		    (< (- (nth 1 articles) (car articles)) nntp-nov-gap))
	  (setq articles (cdr articles)))

	(setq in-process-buffer-p (stringp nntp-server-xover))
        (nntp-send-xover-command first (setq last (car articles)))
        (setq articles (cdr articles))

	(when (and nntp-server-xover in-process-buffer-p)
	  ;; Don't count tried request.
	  (setq count (1+ count))

	  ;; Every 400 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or (null articles)	;All requests have been sent.
		    (= 1 (% count nntp-maximum-request)))

	    (nntp-accept-response)
	    ;; On some Emacs versions the preceding function has a
	    ;; tendency to change the buffer.  Perhaps.  It's quite
	    ;; difficult to reproduce, because it only seems to happen
	    ;; once in a blue moon.
	    (set-buffer process-buffer)
	    (while (progn
		     (goto-char (or last-point (point-min)))
		     ;; Count replies.
		     (while (re-search-forward "^\\([0-9][0-9][0-9]\\) .*\n"
					       nil t)
		       (incf received)
		       (setq status (match-string 1))
		       (if (string-match "^[45]" status)
			   (setq status 'error)
			 (setq status 'ok)))
		     (setq last-point (point))
		     (or (< received count)
			 (if (eq status 'error)
			     nil
			   ;; I haven't started reading the final response
			   (progn
			     (goto-char (point-max))
			     (forward-line -1)
			     (not (looking-at "^\\.\r?\n"))))))
	      ;; I haven't read the end of the final response
	      (nntp-accept-response)
	      (set-buffer process-buffer))))

        ;; Some nntp servers seem to have an extension to the XOVER
        ;; extension.  On these servers, requesting an article range
        ;; preceding the active range does not return an error as
        ;; specified in the RFC.  What we instead get is the NOV entry
        ;; for the first available article.  Obviously, a client can
        ;; use that entry to avoid making unnecessary requests.  The
        ;; only problem is for a client that assumes that the response
        ;; will always be within the requested range.  For such a
        ;; client, we can get N copies of the same entry (one for each
        ;; XOVER command sent to the server).

        (when (<= count 1)
          (goto-char (point-min))
          (when (re-search-forward "^[0-9][0-9][0-9] .*\n\\([0-9]+\\)" nil t)
            (let ((low-limit (string-to-number
			      (buffer-substring (match-beginning 1)
						(match-end 1)))))
              (while (and articles (<= (car articles) low-limit))
                (setq articles (cdr articles))))))
        (set-buffer buf))

      (when nntp-server-xover
	(when in-process-buffer-p
	  (set-buffer buf)
	  (goto-char (point-max))
	  (nnheader-insert-buffer-substring process-buffer)
	  (set-buffer process-buffer)
	  (erase-buffer)
	  (set-buffer buf))

	;; We remove any "." lines and status lines.
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (delete-char -1))
	(goto-char (point-min))
	(delete-matching-lines "^\\.$\\|^[1-5][0-9][0-9] ")
	t))))

  nntp-server-xover)

(defun nntp-send-xover-command (beg end &optional wait-for-reply)
  "Send the XOVER command to the server."
  (let ((range (format "%d-%d" beg end))
	(nntp-inhibit-erase t))
    (if (stringp nntp-server-xover)
	;; If `nntp-server-xover' is a string, then we just send this
	;; command.
	(if wait-for-reply
	    (nntp-send-command-nodelete
	     "\r?\n\\.\r?\n" nntp-server-xover range)
	  ;; We do not wait for the reply.
	  (nntp-send-command-nodelete nil nntp-server-xover range))
      (let ((commands nntp-xover-commands))
	;; `nntp-xover-commands' is a list of possible XOVER commands.
	;; We try them all until we get at positive response.
	(while (and commands (eq nntp-server-xover 'try))
	  (nntp-send-command-nodelete "\r?\n\\.\r?\n" (car commands) range)
	  (with-current-buffer nntp-server-buffer
	    (goto-char (point-min))
	    (and (looking-at "[23]")	; No error message.
		 ;; We also have to look at the lines.  Some buggy
		 ;; servers give back simple lines with just the
		 ;; article number.  How... helpful.
		 (progn
		   (forward-line 1)
		   ;; More text after number, or a dot.
		   (looking-at "[0-9]+\t...\\|\\.\r?\n"))
		 (setq nntp-server-xover (car commands))))
	  (setq commands (cdr commands)))
	;; If none of the commands worked, we disable XOVER.
	(when (eq nntp-server-xover 'try)
	  (nntp-erase-buffer nntp-server-buffer)
	  (setq nntp-server-xover nil))
        nntp-server-xover))))

(defun nntp-find-group-and-number (&optional group)
  (save-excursion
    (save-restriction
      ;; FIXME: This is REALLY FISHY: set-buffer after save-restriction?!?
      (set-buffer nntp-server-buffer)
      (narrow-to-region (goto-char (point-min))
			(or (search-forward "\n\n" nil t) (point-max)))
      (goto-char (point-min))
      ;; We first find the number by looking at the status line.
      (let ((number (and (looking-at "2[0-9][0-9] +\\([0-9]+\\) ")
			 (string-to-number
			  (buffer-substring (match-beginning 1)
					    (match-end 1)))))
	    newsgroups xref)
	(and number (zerop number) (setq number nil))
	(if number
	    ;; Then we find the group name.
	    (setq group
		  (cond
		   ;; If there is only one group in the Newsgroups
		   ;; header, then it seems quite likely that this
		   ;; article comes from that group, I'd say.
		   ((and (setq newsgroups
			       (mail-fetch-field "newsgroups"))
			 (not (string-match "," newsgroups)))
		    newsgroups)
		   ;; If there is more than one group in the
		   ;; Newsgroups header, then the Xref header should
		   ;; be filled out.  We hazard a guess that the group
		   ;; that has this article number in the Xref header
		   ;; is the one we are looking for.  This might very
		   ;; well be wrong if this article happens to have
		   ;; the same number in several groups, but that's
		   ;; life.
		   ((and (setq xref (mail-fetch-field "xref"))
			 number
			 (string-match
			  (format "\\([^ :]+\\):%d" number) xref))
		    (match-string 1 xref))
		   (t "")))
	  (cond
	   ((and (not nntp-xref-number-is-evil)
		 (setq xref (mail-fetch-field "xref"))
		 (string-match
		  (if group
		      (concat "\\(" (regexp-quote group) "\\):\\([0-9]+\\)")
		    "\\([^ :]+\\):\\([0-9]+\\)")
		  xref))
	    (setq group (match-string 1 xref)
		  number (string-to-number (match-string 2 xref))))
	   ((and (setq newsgroups
		       (mail-fetch-field "newsgroups"))
		 (not (string-match "," newsgroups)))
	    (setq group newsgroups))
	   (group)
	   (t (setq group ""))))
	(when (string-match "\r" group)
	  (setq group (substring group 0 (match-beginning 0))))
	(cons group number)))))

(defun nntp-wait-for-string (regexp)
  "Wait until string arrives in the buffer."
  (let ((buf (current-buffer))
	proc)
    (goto-char (point-min))
    (while (and (setq proc (get-buffer-process buf))
		(memq (process-status proc) '(open run))
		(not (re-search-forward regexp nil t)))
      (accept-process-output proc 0.1)
      (set-buffer buf)
      (goto-char (point-min)))))


;; ==========================================================================
;; Obsolete nntp-open-* connection methods -- drv
;; ==========================================================================

(defvoo nntp-open-telnet-envuser nil
  "*If non-nil, telnet session (client and server both) will support the ENVIRON option and not prompt for login name.")

(defvoo nntp-telnet-shell-prompt "bash\\|\$ *\r?$\\|> *\r?"
  "*Regular expression to match the shell prompt on the remote machine.")

(defvoo nntp-rlogin-program "rsh"
  "*Program used to log in on remote machines.
The default is \"rsh\", but \"ssh\" is a popular alternative.")

(defvoo nntp-rlogin-parameters '("telnet" "-8" "${NNTPSERVER:=news}" "nntp")
  "*Parameters to `nntp-open-rlogin'.
That function may be used as `nntp-open-connection-function'.  In that
case, this list will be used as the parameter list given to rsh.")

(defvoo nntp-rlogin-user-name nil
  "*User name on remote system when using the rlogin connect method.")

(defvoo nntp-telnet-parameters
    '("exec" "telnet" "-8" "${NNTPSERVER:=news}" "nntp")
  "*Parameters to `nntp-open-telnet'.
That function may be used as `nntp-open-connection-function'.  In that
case, this list will be executed as a command after logging in
via telnet.")

(defvoo nntp-telnet-user-name nil
  "User name to log in via telnet with.")

(defvoo nntp-telnet-passwd nil
  "Password to use to log in via telnet with.")

(defun nntp-service-to-port (svc)
  (cond
   ((integerp svc) (number-to-string svc))
   ((string-match "\\`[0-9]+\\'" svc) svc)
   (t
    (with-temp-buffer
      (ignore-errors (insert-file-contents "/etc/services"))
      (goto-char (point-min))
      (if (re-search-forward (concat "^" (regexp-quote svc)
                                     "[ \t]+\\([0-9]+\\)/tcp"))
          (match-string 1)
        svc)))))

(defun nntp-open-telnet (buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (let ((proc (apply
		 'start-process
		 "nntpd" buffer nntp-telnet-command nntp-telnet-switches))
	  (case-fold-search t))
      (when (memq (process-status proc) '(open run))
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "set escape \^X\n")
	(cond
	 ((and nntp-open-telnet-envuser nntp-telnet-user-name)
	  (process-send-string proc (concat "open " "-l" nntp-telnet-user-name
					    nntp-address "\n")))
	 (t
	  (process-send-string proc (concat "open " nntp-address "\n"))))
	(cond
	 ((not nntp-open-telnet-envuser)
	  (nntp-wait-for-string "^\r*.?login:")
	  (process-send-string
	   proc (concat
		 (or nntp-telnet-user-name
		     (setq nntp-telnet-user-name (read-string "login: ")))
		 "\n"))))
	(nntp-wait-for-string "^\r*.?password:")
	(process-send-string
	 proc (concat
	       (or nntp-telnet-passwd
		   (setq nntp-telnet-passwd
			 (read-passwd "Password: ")))
	       "\n"))
	(nntp-wait-for-string nntp-telnet-shell-prompt)
	(process-send-string
	 proc (concat (mapconcat 'identity nntp-telnet-parameters " ") "\n"))
	(nntp-wait-for-string "^\r*20[01]")
	(beginning-of-line)
	(delete-region (point-min) (point))
	(process-send-string proc "\^]")
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "mode character\n")
	(accept-process-output proc 1)
	(sit-for 1)
	(goto-char (point-min))
	(forward-line 1)
	(delete-region (point) (point-max)))
      proc)))

(defun nntp-open-rlogin (buffer)
  "Open a connection to SERVER using rsh."
  (let ((proc (if nntp-rlogin-user-name
		  (apply 'start-process
			 "nntpd" buffer nntp-rlogin-program
			 nntp-address "-l" nntp-rlogin-user-name
			 nntp-rlogin-parameters)
		(apply 'start-process
		       "nntpd" buffer nntp-rlogin-program nntp-address
		       nntp-rlogin-parameters))))
    (with-current-buffer buffer
      (nntp-wait-for-string "^\r*20[01]")
      (beginning-of-line)
      (delete-region (point-min) (point))
      proc)))


;; ==========================================================================
;; Replacements for the nntp-open-* functions -- drv
;; ==========================================================================

(defun nntp-open-telnet-stream (buffer)
  "Open a nntp connection by telnet'ing the news server.
`nntp-open-netcat-stream' is recommended in place of this function
because it is more reliable.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-telnet-command',
- `nntp-telnet-switches',
- `nntp-address',
- `nntp-port-number',
- `nntp-end-of-line'."
  (let ((command `(,nntp-telnet-command
		   ,@nntp-telnet-switches
		   ,nntp-address
		   ,(nntp-service-to-port nntp-port-number)))
	proc)
    (and nntp-pre-command
	 (push nntp-pre-command command))
    (setq proc (apply 'start-process "nntpd" buffer command))
    (with-current-buffer buffer
      (nntp-wait-for-string "^\r*20[01]")
      (beginning-of-line)
      (delete-region (point-min) (point))
      proc)))

(defun nntp-open-via-rlogin-and-telnet (buffer)
  "Open a connection to an nntp server through an intermediate host.
First rlogin to the remote host, and then telnet the real news server
from there.
`nntp-open-via-rlogin-and-netcat' is recommended in place of this function
because it is more reliable.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-via-rlogin-command',
- `nntp-via-rlogin-command-switches',
- `nntp-via-user-name',
- `nntp-via-address',
- `nntp-telnet-command',
- `nntp-telnet-switches',
- `nntp-address',
- `nntp-port-number',
- `nntp-end-of-line'."
  (let ((command `(,nntp-via-address
		   ,nntp-telnet-command
		   ,@nntp-telnet-switches))
	proc)
    (when nntp-via-user-name
      (setq command `("-l" ,nntp-via-user-name ,@command)))
    (when nntp-via-rlogin-command-switches
      (setq command (append nntp-via-rlogin-command-switches command)))
    (push nntp-via-rlogin-command command)
    (and nntp-pre-command
	 (push nntp-pre-command command))
    (setq proc (apply 'start-process "nntpd" buffer command))
    (with-current-buffer buffer
      (nntp-wait-for-string "^r?telnet")
      (process-send-string proc (concat "open " nntp-address " "
					(nntp-service-to-port nntp-port-number)
					"\n"))
      (nntp-wait-for-string "^\r*20[01]")
      (beginning-of-line)
      (delete-region (point-min) (point))
      (process-send-string proc "\^]")
      (nntp-wait-for-string "^r?telnet")
      (process-send-string proc "mode character\n")
      (accept-process-output proc 1)
      (sit-for 1)
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (point-max)))
    proc))

(defun nntp-open-via-rlogin-and-netcat (buffer)
  "Open a connection to an nntp server through an intermediate host.
First rlogin to the remote host, and then connect to the real news
server from there using the netcat command.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-via-rlogin-command',
- `nntp-via-rlogin-command-switches',
- `nntp-via-user-name',
- `nntp-via-address',
- `nntp-netcat-command',
- `nntp-netcat-switches',
- `nntp-address',
- `nntp-port-number'."
  (let ((command `(,@(when nntp-pre-command
		       (list nntp-pre-command))
		   ,nntp-via-rlogin-command
		   ,@nntp-via-rlogin-command-switches
		   ,@(when nntp-via-user-name
		       (list "-l" nntp-via-user-name))
		   ,nntp-via-address
		   ,nntp-netcat-command
		   ,@nntp-netcat-switches
		   ,nntp-address
		   ,(nntp-service-to-port nntp-port-number))))
    ;; A non-nil connection type results in mightily odd behavior where
    ;; (process-send-string proc "\^M") ends up sending a "\n" to the
    ;; ssh process.  --Stef
    ;; Also a nil connection allow ssh-askpass to work under X11.
    (let ((process-connection-type nil))
      (apply 'start-process "nntpd" buffer command))))

(defun nntp-open-netcat-stream (buffer)
  "Open a connection to an nntp server through netcat.
I.e. use the `nc' command rather than Emacs's builtin networking code.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-netcat-command',
- `nntp-netcat-switches',
- `nntp-address',
- `nntp-port-number'."
  (let ((command `(,nntp-netcat-command
		   ,@nntp-netcat-switches
                   ,nntp-address
                   ,(nntp-service-to-port nntp-port-number))))
    (and nntp-pre-command (push nntp-pre-command command))
    (let ((process-connection-type nil)) ;See `nntp-open-via-rlogin-and-netcat'.
      (apply 'start-process "nntpd" buffer command))))


(defun nntp-open-via-telnet-and-telnet (buffer)
  "Open a connection to an nntp server through an intermediate host.
First telnet the remote host, and then telnet the real news server
from there.

Please refer to the following variables to customize the connection:
- `nntp-pre-command',
- `nntp-via-telnet-command',
- `nntp-via-telnet-switches',
- `nntp-via-address',
- `nntp-via-envuser',
- `nntp-via-user-name',
- `nntp-via-user-password',
- `nntp-via-shell-prompt',
- `nntp-telnet-command',
- `nntp-telnet-switches',
- `nntp-address',
- `nntp-port-number',
- `nntp-end-of-line'."
  (with-current-buffer buffer
    (erase-buffer)
    (let ((command `(,nntp-via-telnet-command ,@nntp-via-telnet-switches))
	  (case-fold-search t)
	  proc)
      (and nntp-pre-command (push nntp-pre-command command))
      (setq proc (apply 'start-process "nntpd" buffer command))
      (when (memq (process-status proc) '(open run))
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "set escape \^X\n")
	(cond
	 ((and nntp-via-envuser nntp-via-user-name)
	  (process-send-string proc (concat "open " "-l" nntp-via-user-name
					    nntp-via-address "\n")))
	 (t
	  (process-send-string proc (concat "open " nntp-via-address
					    "\n"))))
	(when (not nntp-via-envuser)
	  (nntp-wait-for-string "^\r*.?login:")
	  (process-send-string proc
			       (concat
				(or nntp-via-user-name
				    (setq nntp-via-user-name
					  (read-string "login: ")))
				"\n")))
	(nntp-wait-for-string "^\r*.?password:")
	(process-send-string proc
			     (concat
			      (or nntp-via-user-password
				  (setq nntp-via-user-password
					(read-passwd "Password: ")))
			      "\n"))
	(nntp-wait-for-string nntp-via-shell-prompt)
	(let ((real-telnet-command `("exec"
				     ,nntp-telnet-command
				     ,@nntp-telnet-switches
				     ,nntp-address
				     ,(nntp-service-to-port nntp-port-number))))
	  (process-send-string proc
			       (concat (mapconcat 'identity
						  real-telnet-command " ")
				       "\n")))
	(nntp-wait-for-string "^\r*20[01]")
	(beginning-of-line)
	(delete-region (point-min) (point))
	(process-send-string proc "\^]")
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "mode character\n")
	(accept-process-output proc 1)
	(sit-for 1)
	(goto-char (point-min))
	(forward-line 1)
	(delete-region (point) (point-max)))
      proc)))

;; Marks handling

(defun nntp-marks-directory (server)
  (expand-file-name server nntp-marks-directory))

(defvar nntp-server-to-method-cache nil
  "Alist of servers and select methods.")

(defun nntp-group-pathname (server group &optional file)
  "Return an absolute file name of FILE for GROUP on SERVER."
  (let ((method (cdr (assoc server nntp-server-to-method-cache))))
    (unless method
      (push (cons server (setq method (or (gnus-server-to-method server)
					  (gnus-find-method-for-group group))))
	    nntp-server-to-method-cache))
    (nnmail-group-pathname
     (mm-decode-coding-string group
			      (inline (gnus-group-name-charset method group)))
     (nntp-marks-directory server)
     file)))

(defun nntp-possibly-create-directory (group server)
  (let ((dir (nntp-group-pathname server group))
	(file-name-coding-system nnmail-pathname-coding-system))
    (unless (file-exists-p dir)
      (make-directory (directory-file-name dir) t)
      (nnheader-message 5 "Creating nntp marks directory %s" dir))))

(autoload 'time-less-p "time-date")

(defun nntp-marks-changed-p (group server)
  (let ((file (nntp-group-pathname server group nntp-marks-file-name))
	(file-name-coding-system nnmail-pathname-coding-system))
    (if (null (gnus-gethash file nntp-marks-modtime))
	t ;; never looked at marks file, assume it has changed
      (time-less-p (gnus-gethash file nntp-marks-modtime)
		   (nth 5 (file-attributes file))))))

(defun nntp-save-marks (group server)
  (let ((file-name-coding-system nnmail-pathname-coding-system)
	(file (nntp-group-pathname server group nntp-marks-file-name)))
    (condition-case err
	(progn
	  (nntp-possibly-create-directory group server)
	  (with-temp-file file
	    (erase-buffer)
	    (gnus-prin1 nntp-marks)
	    (insert "\n"))
	  (gnus-sethash file
			(nth 5 (file-attributes file))
			nntp-marks-modtime))
      (error (or (gnus-yes-or-no-p
		  (format "Could not write to %s (%s).  Continue? " file err))
		 (error "Cannot write to %s (%s)" file err))))))

(defun nntp-open-marks (group server)
  (let ((file (nntp-group-pathname server group nntp-marks-file-name))
	(file-name-coding-system nnmail-pathname-coding-system))
    (if (file-exists-p file)
	(condition-case err
	    (with-temp-buffer
	      (gnus-sethash file (nth 5 (file-attributes file))
			    nntp-marks-modtime)
	      (nnheader-insert-file-contents file)
	      (setq nntp-marks (read (current-buffer)))
	      (dolist (el gnus-article-unpropagated-mark-lists)
		(setq nntp-marks (gnus-remassoc el nntp-marks))))
	  (error (or (gnus-yes-or-no-p
		      (format "Error reading nntp marks file %s (%s).  Continuing will use marks from .newsrc.eld.  Continue? " file err))
		     (error "Cannot read nntp marks file %s (%s)" file err))))
      ;; User didn't have a .marks file.  Probably first time
      ;; user of the .marks stuff.  Bootstrap it from .newsrc.eld.
      (let ((info (gnus-get-info
		   (gnus-group-prefixed-name
		    group
		    (gnus-server-to-method (format "nntp:%s" server)))))
	    (decoded-name (mm-decode-coding-string
			   group
			   (gnus-group-name-charset
			    (gnus-server-to-method server) group))))
	(nnheader-message 7 "Bootstrapping marks for %s..." decoded-name)
	(setq nntp-marks (gnus-info-marks info))
	(push (cons 'read (gnus-info-read info)) nntp-marks)
	(dolist (el gnus-article-unpropagated-mark-lists)
	  (setq nntp-marks (gnus-remassoc el nntp-marks)))
	(nntp-save-marks group server)
	(nnheader-message 7 "Bootstrapping marks for %s...done"
			  decoded-name)))))

(provide 'nntp)

;;; nntp.el ends here
