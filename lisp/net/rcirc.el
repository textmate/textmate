;;; rcirc.el --- default, simple IRC client.

;; Copyright (C) 2005-2012 Free Software Foundation, Inc.

;; Author: Ryan Yeske <rcyeske@gmail.com>
;; Maintainers: Ryan Yeske <rcyeske@gmail.com>,
;;              Deniz Dogan <deniz@dogan.se>
;; Keywords: comm

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

;; Internet Relay Chat (IRC) is a form of instant communication over
;; the Internet. It is mainly designed for group (many-to-many)
;; communication in discussion forums called channels, but also allows
;; one-to-one communication.

;; Rcirc has simple defaults and clear and consistent behavior.
;; Message arrival timestamps, activity notification on the modeline,
;; message filling, nick completion, and keepalive pings are all
;; enabled by default, but can easily be adjusted or turned off.  Each
;; discussion takes place in its own buffer and there is a single
;; server buffer per connection.

;; Open a new irc connection with:
;; M-x irc RET

;;; Todo:

;;; Code:

(require 'ring)
(require 'time-date)
(eval-when-compile (require 'cl))

(defgroup rcirc nil
  "Simple IRC client."
  :version "22.1"
  :prefix "rcirc-"
  :link '(custom-manual "(rcirc)")
  :group 'applications)

(defcustom rcirc-server-alist
  '(("irc.freenode.net" :channels ("#rcirc")
     ;; Don't use the TLS port by default, in case gnutls is not available.
     ;; :port 7000 :encryption tls
     ))
  "An alist of IRC connections to establish when running `rcirc'.
Each element looks like (SERVER-NAME PARAMETERS).

SERVER-NAME is a string describing the server to connect
to.

The optional PARAMETERS come in pairs PARAMETER VALUE.

The following parameters are recognized:

`:nick'

VALUE must be a string.  If absent, `rcirc-default-nick' is used
for this connection.

`:port'

VALUE must be a number or string.  If absent,
`rcirc-default-port' is used.

`:user-name'

VALUE must be a string.  If absent, `rcirc-default-user-name' is
used.

`:password'

VALUE must be a string.  If absent, no PASS command will be sent
to the server.

`:full-name'

VALUE must be a string.  If absent, `rcirc-default-full-name' is
used.

`:channels'

VALUE must be a list of strings describing which channels to join
when connecting to this server.  If absent, no channels will be
connected to automatically.

`:encryption'

VALUE must be `plain' (the default) for unencrypted connections, or `tls'
for connections using SSL/TLS."
  :type '(alist :key-type string
		:value-type (plist :options
                                   ((:nick string)
                                    (:port integer)
                                    (:user-name string)
                                    (:password string)
                                    (:full-name string)
                                    (:channels (repeat string))
                                    (:encryption (choice (const tls)
                                                         (const plain))))))
  :group 'rcirc)

(defcustom rcirc-default-port 6667
  "The default port to connect to."
  :type 'integer
  :group 'rcirc)

(defcustom rcirc-default-nick (user-login-name)
  "Your nick."
  :type 'string
  :group 'rcirc)

(defcustom rcirc-default-user-name "user"
  "Your user name sent to the server when connecting."
  :version "24.1"                       ; changed default
  :type 'string
  :group 'rcirc)

(defcustom rcirc-default-full-name "unknown"
  "The full name sent to the server when connecting."
  :version "24.1"                       ; changed default
  :type 'string
  :group 'rcirc)

(defcustom rcirc-fill-flag t
  "*Non-nil means line-wrap messages printed in channel buffers."
  :type 'boolean
  :group 'rcirc)

(defcustom rcirc-fill-column nil
  "*Column beyond which automatic line-wrapping should happen.
If nil, use value of `fill-column'.  If 'frame-width, use the
maximum frame width."
  :type '(choice (const :tag "Value of `fill-column'")
		 (const :tag "Full frame width" frame-width)
		 (integer :tag "Number of columns"))
  :group 'rcirc)

(defcustom rcirc-fill-prefix nil
  "*Text to insert before filled lines.
If nil, calculate the prefix dynamically to line up text
underneath each nick."
  :type '(choice (const :tag "Dynamic" nil)
		 (string :tag "Prefix text"))
  :group 'rcirc)

(defvar rcirc-ignore-buffer-activity-flag nil
  "If non-nil, ignore activity in this buffer.")
(make-variable-buffer-local 'rcirc-ignore-buffer-activity-flag)

(defvar rcirc-low-priority-flag nil
  "If non-nil, activity in this buffer is considered low priority.")
(make-variable-buffer-local 'rcirc-low-priority-flag)

(defvar rcirc-omit-mode nil
  "Non-nil if Rcirc-Omit mode is enabled.
Use the command `rcirc-omit-mode' to change this variable.")
(make-variable-buffer-local 'rcirc-omit-mode)

(defcustom rcirc-time-format "%H:%M "
  "*Describes how timestamps are printed.
Used as the first arg to `format-time-string'."
  :type 'string
  :group 'rcirc)

(defcustom rcirc-input-ring-size 1024
  "*Size of input history ring."
  :type 'integer
  :group 'rcirc)

(defcustom rcirc-read-only-flag t
  "*Non-nil means make text in IRC buffers read-only."
  :type 'boolean
  :group 'rcirc)

(defcustom rcirc-buffer-maximum-lines nil
  "*The maximum size in lines for rcirc buffers.
Channel buffers are truncated from the top to be no greater than this
number.  If zero or nil, no truncating is done."
  :type '(choice (const :tag "No truncation" nil)
		 (integer :tag "Number of lines"))
  :group 'rcirc)

(defcustom rcirc-scroll-show-maximum-output t
  "*If non-nil, scroll buffer to keep the point at the bottom of
the window."
  :type 'boolean
  :group 'rcirc)

(defcustom rcirc-authinfo nil
  "List of authentication passwords.
Each element of the list is a list with a SERVER-REGEXP string
and a method symbol followed by method specific arguments.

The valid METHOD symbols are `nickserv', `chanserv' and
`bitlbee'.

The ARGUMENTS for each METHOD symbol are:
  `nickserv': NICK PASSWORD [NICKSERV-NICK]
  `chanserv': NICK CHANNEL PASSWORD
  `bitlbee': NICK PASSWORD
  `quakenet': ACCOUNT PASSWORD

Examples:
 ((\"freenode\" nickserv \"bob\" \"p455w0rd\")
  (\"freenode\" chanserv \"bob\" \"#bobland\" \"passwd99\")
  (\"bitlbee\" bitlbee \"robert\" \"sekrit\")
  (\"dal.net\" nickserv \"bob\" \"sekrit\" \"NickServ@services.dal.net\")
  (\"quakenet.org\" quakenet \"bobby\" \"sekrit\"))"
  :type '(alist :key-type (string :tag "Server")
		:value-type (choice (list :tag "NickServ"
					  (const nickserv)
					  (string :tag "Nick")
					  (string :tag "Password"))
				    (list :tag "ChanServ"
					  (const chanserv)
					  (string :tag "Nick")
					  (string :tag "Channel")
					  (string :tag "Password"))
				    (list :tag "BitlBee"
					  (const bitlbee)
					  (string :tag "Nick")
					  (string :tag "Password"))
                                    (list :tag "QuakeNet"
                                          (const quakenet)
                                          (string :tag "Account")
                                          (string :tag "Password"))))
  :group 'rcirc)

(defcustom rcirc-auto-authenticate-flag t
  "*Non-nil means automatically send authentication string to server.
See also `rcirc-authinfo'."
  :type 'boolean
  :group 'rcirc)

(defcustom rcirc-authenticate-before-join t
  "*Non-nil means authenticate to services before joining channels.
Currently only works with NickServ on some networks."
  :version "24.1"
  :type 'boolean
  :group 'rcirc)

(defcustom rcirc-prompt "> "
  "Prompt string to use in IRC buffers.

The following replacements are made:
%n is your nick.
%s is the server.
%t is the buffer target, a channel or a user.

Setting this alone will not affect the prompt;
use either M-x customize or also call `rcirc-update-prompt'."
  :type 'string
  :set 'rcirc-set-changed
  :initialize 'custom-initialize-default
  :group 'rcirc)

(defcustom rcirc-keywords nil
  "List of keywords to highlight in message text."
  :type '(repeat string)
  :group 'rcirc)

(defcustom rcirc-ignore-list ()
  "List of ignored nicks.
Use /ignore to list them, use /ignore NICK to add or remove a nick."
  :type '(repeat string)
  :group 'rcirc)

(defvar rcirc-ignore-list-automatic ()
  "List of ignored nicks added to `rcirc-ignore-list' because of renaming.
When an ignored person renames, their nick is added to both lists.
Nicks will be removed from the automatic list on follow-up renamings or
parts.")

(defcustom rcirc-bright-nicks nil
  "List of nicks to be emphasized.
See `rcirc-bright-nick' face."
  :type '(repeat string)
  :group 'rcirc)

(defcustom rcirc-dim-nicks nil
  "List of nicks to be deemphasized.
See `rcirc-dim-nick' face."
  :type '(repeat string)
  :group 'rcirc)

(defcustom rcirc-print-hooks nil
  "Hook run after text is printed.
Called with 5 arguments, PROCESS, SENDER, RESPONSE, TARGET and TEXT."
  :type 'hook
  :group 'rcirc)

(defvar rcirc-authenticated-hook nil
  "Hook run after successfully authenticated.")

(defcustom rcirc-always-use-server-buffer-flag nil
  "Non-nil means messages without a channel target will go to the server buffer."
  :type 'boolean
  :group 'rcirc)

(defcustom rcirc-decode-coding-system 'utf-8
  "Coding system used to decode incoming irc messages.
Set to 'undecided if you want the encoding of the incoming
messages autodetected."
  :type 'coding-system
  :group 'rcirc)

(defcustom rcirc-encode-coding-system 'utf-8
  "Coding system used to encode outgoing irc messages."
  :type 'coding-system
  :group 'rcirc)

(defcustom rcirc-coding-system-alist nil
  "Alist to decide a coding system to use for a channel I/O operation.
The format is ((PATTERN . VAL) ...).
PATTERN is either a string or a cons of strings.
If PATTERN is a string, it is used to match a target.
If PATTERN is a cons of strings, the car part is used to match a
target, and the cdr part is used to match a server.
VAL is either a coding system or a cons of coding systems.
If VAL is a coding system, it is used for both decoding and encoding
messages.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding."
  :type '(alist :key-type (choice (string :tag "Channel Regexp")
					  (cons (string :tag "Channel Regexp")
						(string :tag "Server Regexp")))
		:value-type (choice coding-system
				    (cons (coding-system :tag "Decode")
					  (coding-system :tag "Encode"))))
  :group 'rcirc)

(defcustom rcirc-multiline-major-mode 'fundamental-mode
  "Major-mode function to use in multiline edit buffers."
  :type 'function
  :group 'rcirc)

(defcustom rcirc-nick-completion-format "%s: "
  "Format string to use in nick completions.

The format string is only used when completing at the beginning
of a line.  The string is passed as the first argument to
`format' with the nickname as the second argument."
  :version "24.1"
  :type 'string
  :group 'rcirc)

(defvar rcirc-nick nil)

(defvar rcirc-prompt-start-marker nil)
(defvar rcirc-prompt-end-marker nil)

(defvar rcirc-nick-table nil)

(defvar rcirc-recent-quit-alist nil
  "Alist of nicks that have recently quit or parted the channel.")

(defvar rcirc-nick-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (mapc (lambda (c) (modify-syntax-entry c "w" table))
          "[]\\`_^{|}-")
    (modify-syntax-entry ?' "_" table)
    table)
  "Syntax table which includes all nick characters as word constituents.")

;; each process has an alist of (target . buffer) pairs
(defvar rcirc-buffer-alist nil)

(defvar rcirc-activity nil
  "List of buffers with unviewed activity.")

(defvar rcirc-activity-string ""
  "String displayed in modeline representing `rcirc-activity'.")
(put 'rcirc-activity-string 'risky-local-variable t)

(defvar rcirc-server-buffer nil
  "The server buffer associated with this channel buffer.")

(defvar rcirc-target nil
  "The channel or user associated with this buffer.")

(defvar rcirc-urls nil
  "List of urls seen in the current buffer.")
(put 'rcirc-urls 'permanent-local t)

(defvar rcirc-timeout-seconds 600
  "Kill connection after this many seconds if there is no activity.")

(defconst rcirc-id-string (concat "rcirc on GNU Emacs " emacs-version))

(defvar rcirc-startup-channels nil)

(defvar rcirc-server-name-history nil
  "History variable for \\[rcirc] call.")

(defvar rcirc-server-port-history nil
  "History variable for \\[rcirc] call.")

(defvar rcirc-nick-name-history nil
  "History variable for \\[rcirc] call.")

(defvar rcirc-user-name-history nil
  "History variable for \\[rcirc] call.")

;;;###autoload
(defun rcirc (arg)
  "Connect to all servers in `rcirc-server-alist'.

Do not connect to a server if it is already connected.

If ARG is non-nil, instead prompt for connection parameters."
  (interactive "P")
  (if arg
      (let* ((server (completing-read "IRC Server: "
				      rcirc-server-alist
				      nil nil
				      (caar rcirc-server-alist)
				      'rcirc-server-name-history))
	     (server-plist (cdr (assoc-string server rcirc-server-alist)))
	     (port (read-string "IRC Port: "
				(number-to-string
				 (or (plist-get server-plist :port)
				     rcirc-default-port))
				'rcirc-server-port-history))
	     (nick (read-string "IRC Nick: "
				(or (plist-get server-plist :nick)
				    rcirc-default-nick)
				'rcirc-nick-name-history))
	     (user-name (read-string "IRC Username: "
                                     (or (plist-get server-plist :user-name)
                                         rcirc-default-user-name)
                                     'rcirc-user-name-history))
	     (password (read-passwd "IRC Password: " nil
                                    (plist-get server-plist :password)))
	     (channels (split-string
			(read-string "IRC Channels: "
				     (mapconcat 'identity
						(plist-get server-plist
							   :channels)
						" "))
			"[, ]+" t))
             (encryption (rcirc-prompt-for-encryption server-plist)))
	(rcirc-connect server port nick user-name
		       rcirc-default-full-name
		       channels password encryption))
    ;; connect to servers in `rcirc-server-alist'
    (let (connected-servers)
      (dolist (c rcirc-server-alist)
	(let ((server (car c))
	      (nick (or (plist-get (cdr c) :nick) rcirc-default-nick))
	      (port (or (plist-get (cdr c) :port) rcirc-default-port))
	      (user-name (or (plist-get (cdr c) :user-name)
			     rcirc-default-user-name))
	      (full-name (or (plist-get (cdr c) :full-name)
			     rcirc-default-full-name))
	      (channels (plist-get (cdr c) :channels))
              (password (plist-get (cdr c) :password))
              (encryption (plist-get (cdr c) :encryption)))
	  (when server
	    (let (connected)
	      (dolist (p (rcirc-process-list))
		(when (string= server (process-name p))
		  (setq connected p)))
	      (if (not connected)
		  (condition-case e
		      (rcirc-connect server port nick user-name
				     full-name channels password encryption)
		    (quit (message "Quit connecting to %s" server)))
		(with-current-buffer (process-buffer connected)
		  (setq connected-servers
			(cons (process-contact (get-buffer-process
						(current-buffer)) :host)
			      connected-servers))))))))
      (when connected-servers
	(message "Already connected to %s"
		 (if (cdr connected-servers)
		     (concat (mapconcat 'identity (butlast connected-servers) ", ")
			     ", and "
			     (car (last connected-servers)))
		   (car connected-servers)))))))

;;;###autoload
(defalias 'irc 'rcirc)


(defvar rcirc-process-output nil)
(defvar rcirc-topic nil)
(defvar rcirc-keepalive-timer nil)
(defvar rcirc-last-server-message-time nil)
(defvar rcirc-server nil)		; server provided by server
(defvar rcirc-server-name nil)		; server name given by 001 response
(defvar rcirc-timeout-timer nil)
(defvar rcirc-user-authenticated nil)
(defvar rcirc-user-disconnect nil)
(defvar rcirc-connecting nil)
(defvar rcirc-process nil)

;;;###autoload
(defun rcirc-connect (server &optional port nick user-name
                             full-name startup-channels password encryption)
  (save-excursion
    (message "Connecting to %s..." server)
    (let* ((inhibit-eol-conversion)
           (port-number (if port
			    (if (stringp port)
				(string-to-number port)
			      port)
			  rcirc-default-port))
	   (nick (or nick rcirc-default-nick))
	   (user-name (or user-name rcirc-default-user-name))
	   (full-name (or full-name rcirc-default-full-name))
	   (startup-channels startup-channels)
           (process (open-network-stream
                     server nil server port-number
                     :type (or encryption 'plain))))
      ;; set up process
      (set-process-coding-system process 'raw-text 'raw-text)
      (switch-to-buffer (rcirc-generate-new-buffer-name process nil))
      (set-process-buffer process (current-buffer))
      (rcirc-mode process nil)
      (set-process-sentinel process 'rcirc-sentinel)
      (set-process-filter process 'rcirc-filter)

      (set (make-local-variable 'rcirc-process) process)
      (set (make-local-variable 'rcirc-server) server)
      (set (make-local-variable 'rcirc-server-name) server) ; Update when we get 001 response.
      (set (make-local-variable 'rcirc-buffer-alist) nil)
      (set (make-local-variable 'rcirc-nick-table)
           (make-hash-table :test 'equal))
      (set (make-local-variable 'rcirc-nick) nick)
      (set (make-local-variable 'rcirc-process-output) nil)
      (set (make-local-variable 'rcirc-startup-channels) startup-channels)
      (set (make-local-variable 'rcirc-last-server-message-time)
           (current-time))

      (set (make-local-variable 'rcirc-timeout-timer) nil)
      (set (make-local-variable 'rcirc-user-disconnect) nil)
      (set (make-local-variable 'rcirc-user-authenticated) nil)
      (set (make-local-variable 'rcirc-connecting) t)

      (add-hook 'auto-save-hook 'rcirc-log-write)

      ;; identify
      (unless (zerop (length password))
        (rcirc-send-string process (concat "PASS " password)))
      (rcirc-send-string process (concat "NICK " nick))
      (rcirc-send-string process (concat "USER " user-name
                                         " 0 * :" full-name))

      ;; setup ping timer if necessary
      (unless rcirc-keepalive-timer
	(setq rcirc-keepalive-timer
	      (run-at-time 0 (/ rcirc-timeout-seconds 2) 'rcirc-keepalive)))

      (message "Connecting to %s...done" server)

      ;; return process object
      process)))

(defmacro with-rcirc-process-buffer (process &rest body)
  (declare (indent 1) (debug t))
  `(with-current-buffer (process-buffer ,process)
     ,@body))

(defmacro with-rcirc-server-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(with-current-buffer rcirc-server-buffer
     ,@body))

(defun rcirc-float-time ()
  (if (featurep 'xemacs)
      (time-to-seconds (current-time))
    (float-time)))

(defun rcirc-prompt-for-encryption (server-plist)
  "Prompt the user for the encryption method to use.
SERVER-PLIST is the property list for the server."
  (let ((msg "Encryption (default %s): ")
        (choices '("plain" "tls"))
        (default (or (plist-get server-plist :encryption)
                     'plain)))
    (intern
     (completing-read (format msg default)
                      choices nil t nil nil (symbol-name default)))))

(defun rcirc-keepalive ()
  "Send keep alive pings to active rcirc processes.
Kill processes that have not received a server message since the
last ping."
  (if (rcirc-process-list)
      (mapc (lambda (process)
	      (with-rcirc-process-buffer process
		(when (not rcirc-connecting)
                  (rcirc-send-ctcp process
                                   rcirc-nick
                                   (format "KEEPALIVE %f"
                                           (rcirc-float-time))))))
            (rcirc-process-list))
    ;; no processes, clean up timer
    (cancel-timer rcirc-keepalive-timer)
    (setq rcirc-keepalive-timer nil)))

(defun rcirc-handler-ctcp-KEEPALIVE (process target sender message)
  (with-rcirc-process-buffer process
    (setq header-line-format (format "%f" (- (rcirc-float-time)
					     (string-to-number message))))))

(defvar rcirc-debug-buffer "*rcirc debug*")
(defvar rcirc-debug-flag nil
  "If non-nil, write information to `rcirc-debug-buffer'.")
(defun rcirc-debug (process text)
  "Add an entry to the debug log including PROCESS and TEXT.
Debug text is written to `rcirc-debug-buffer' if `rcirc-debug-flag'
is non-nil."
  (when rcirc-debug-flag
    (with-current-buffer (get-buffer-create rcirc-debug-buffer)
      (goto-char (point-max))
      (insert (concat
	       "["
	       (format-time-string "%Y-%m-%dT%T ") (process-name process)
	       "] "
	       text)))))

(defvar rcirc-sentinel-hooks nil
  "Hook functions called when the process sentinel is called.
Functions are called with PROCESS and SENTINEL arguments.")

(defun rcirc-sentinel (process sentinel)
  "Called when PROCESS receives SENTINEL."
  (let ((sentinel (replace-regexp-in-string "\n" "" sentinel)))
    (rcirc-debug process (format "SENTINEL: %S %S\n" process sentinel))
    (with-rcirc-process-buffer process
      (dolist (buffer (cons nil (mapcar 'cdr rcirc-buffer-alist)))
	(with-current-buffer (or buffer (current-buffer))
	  (rcirc-print process "rcirc.el" "ERROR" rcirc-target
		       (format "%s: %s (%S)"
			       (process-name process)
			       sentinel
			       (process-status process)) (not rcirc-target))
	  (rcirc-disconnect-buffer)))
      (run-hook-with-args 'rcirc-sentinel-hooks process sentinel))))

(defun rcirc-disconnect-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    ;; set rcirc-target to nil for each channel so cleanup
    ;; doesn't happen when we reconnect
    (setq rcirc-target nil)
    (setq mode-line-process ":disconnected")))

(defun rcirc-process-list ()
  "Return a list of rcirc processes."
  (let (ps)
    (mapc (lambda (p)
            (when (buffer-live-p (process-buffer p))
              (with-rcirc-process-buffer p
                (when (eq major-mode 'rcirc-mode)
                  (setq ps (cons p ps))))))
          (process-list))
    ps))

(defvar rcirc-receive-message-hooks nil
  "Hook functions run when a message is received from server.
Function is called with PROCESS, COMMAND, SENDER, ARGS and LINE.")
(defun rcirc-filter (process output)
  "Called when PROCESS receives OUTPUT."
  (rcirc-debug process output)
  (rcirc-reschedule-timeout process)
  (with-rcirc-process-buffer process
    (setq rcirc-last-server-message-time (current-time))
    (setq rcirc-process-output (concat rcirc-process-output output))
    (when (= (aref rcirc-process-output
                   (1- (length rcirc-process-output))) ?\n)
      (mapc (lambda (line)
              (rcirc-process-server-response process line))
            (split-string rcirc-process-output "[\n\r]" t))
      (setq rcirc-process-output nil))))

(defun rcirc-reschedule-timeout (process)
  (with-rcirc-process-buffer process
    (when (not rcirc-connecting)
      (with-rcirc-process-buffer process
	(when rcirc-timeout-timer (cancel-timer rcirc-timeout-timer))
	(setq rcirc-timeout-timer (run-at-time rcirc-timeout-seconds nil
					       'rcirc-delete-process
					       process))))))

(defun rcirc-delete-process (process)
  (delete-process process))

(defvar rcirc-trap-errors-flag t)
(defun rcirc-process-server-response (process text)
  (if rcirc-trap-errors-flag
      (condition-case err
          (rcirc-process-server-response-1 process text)
        (error
         (rcirc-print process "RCIRC" "ERROR" nil
                      (format "\"%s\" %s" text err) t)))
    (rcirc-process-server-response-1 process text)))

(defun rcirc-process-server-response-1 (process text)
  (if (string-match "^\\(:\\([^ ]+\\) \\)?\\([^ ]+\\) \\(.+\\)$" text)
      (let* ((user (match-string 2 text))
	     (sender (rcirc-user-nick user))
             (cmd (match-string 3 text))
             (args (match-string 4 text))
             (handler (intern-soft (concat "rcirc-handler-" cmd))))
        (string-match "^\\([^:]*\\):?\\(.+\\)?$" args)
        (let* ((args1 (match-string 1 args))
               (args2 (match-string 2 args))
               (args (delq nil (append (split-string args1 " " t)
				       (list args2)))))
        (if (not (fboundp handler))
            (rcirc-handler-generic process cmd sender args text)
          (funcall handler process sender args text))
        (run-hook-with-args 'rcirc-receive-message-hooks
                            process cmd sender args text)))
    (message "UNHANDLED: %s" text)))

(defvar rcirc-responses-no-activity '("305" "306")
  "Responses that don't trigger activity in the mode-line indicator.")

(defun rcirc-handler-generic (process response sender args text)
  "Generic server response handler."
  (rcirc-print process sender response nil
               (mapconcat 'identity (cdr args) " ")
	       (not (member response rcirc-responses-no-activity))))

(defun rcirc--connection-open-p (process)
  (memq (process-status process) '(run open)))

(defun rcirc-send-string (process string)
  "Send PROCESS a STRING plus a newline."
  (let ((string (concat (encode-coding-string string rcirc-encode-coding-system)
                        "\n")))
    (unless (rcirc--connection-open-p process)
      (error "Network connection to %s is not open"
             (process-name process)))
    (rcirc-debug process string)
    (process-send-string process string)))

(defun rcirc-send-privmsg (process target string)
  (rcirc-send-string process (format "PRIVMSG %s :%s" target string)))

(defun rcirc-send-ctcp (process target request &optional args)
  (let ((args (if args (concat " " args) "")))
    (rcirc-send-privmsg process target
                        (format "\C-a%s%s\C-a" request args))))

(defun rcirc-buffer-process (&optional buffer)
  "Return the process associated with channel BUFFER.
With no argument or nil as argument, use the current buffer."
  (or (get-buffer-process (if buffer
			      (with-current-buffer buffer
				rcirc-server-buffer)
			    rcirc-server-buffer))
      rcirc-process))

(defun rcirc-server-name (process)
  "Return PROCESS server name, given by the 001 response."
  (with-rcirc-process-buffer process
    (or rcirc-server-name
	(warn "server name for process %S unknown" process))))

(defun rcirc-nick (process)
  "Return PROCESS nick."
  (with-rcirc-process-buffer process
    (or rcirc-nick rcirc-default-nick)))

(defun rcirc-buffer-nick (&optional buffer)
  "Return the nick associated with BUFFER.
With no argument or nil as argument, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (with-current-buffer rcirc-server-buffer
      (or rcirc-nick rcirc-default-nick))))

(defvar rcirc-max-message-length 420
  "Messages longer than this value will be split.")

(defun rcirc-send-message (process target message &optional noticep silent)
  "Send TARGET associated with PROCESS a privmsg with text MESSAGE.
If NOTICEP is non-nil, send a notice instead of privmsg.
If SILENT is non-nil, do not print the message in any irc buffer."
  ;; max message length is 512 including CRLF
  (let* ((response (if noticep "NOTICE" "PRIVMSG"))
         (oversize (> (length message) rcirc-max-message-length))
         (text (if oversize
                   (substring message 0 rcirc-max-message-length)
                 message))
         (text (if (string= text "")
                   " "
                 text))
         (more (if oversize
                   (substring message rcirc-max-message-length))))
    (rcirc-get-buffer-create process target)
    (rcirc-send-string process (concat response " " target " :" text))
    (unless silent
      (rcirc-print process (rcirc-nick process) response target text))
    (when more (rcirc-send-message process target more noticep))))

(defvar rcirc-input-ring nil)
(defvar rcirc-input-ring-index 0)

(defun rcirc-prev-input-string (arg)
  (ring-ref rcirc-input-ring (+ rcirc-input-ring-index arg)))

(defun rcirc-insert-prev-input ()
  (interactive)
  (when (<= rcirc-prompt-end-marker (point))
    (delete-region rcirc-prompt-end-marker (point-max))
    (insert (rcirc-prev-input-string 0))
    (setq rcirc-input-ring-index (1+ rcirc-input-ring-index))))

(defun rcirc-insert-next-input ()
  (interactive)
  (when (<= rcirc-prompt-end-marker (point))
    (delete-region rcirc-prompt-end-marker (point-max))
    (setq rcirc-input-ring-index (1- rcirc-input-ring-index))
    (insert (rcirc-prev-input-string -1))))

(defvar rcirc-server-commands
  '("/admin"   "/away"   "/connect" "/die"      "/error"   "/info"
    "/invite"  "/ison"   "/join"    "/kick"     "/kill"    "/links"
    "/list"    "/lusers" "/mode"    "/motd"     "/names"   "/nick"
    "/notice"  "/oper"   "/part"    "/pass"     "/ping"    "/pong"
    "/privmsg" "/quit"   "/rehash"  "/restart"  "/service" "/servlist"
    "/server"  "/squery" "/squit"   "/stats"    "/summon"  "/time"
    "/topic"   "/trace"  "/user"    "/userhost" "/users"   "/version"
    "/wallops" "/who"    "/whois"   "/whowas")
  "A list of user commands by IRC server.
The value defaults to RFCs 1459 and 2812.")

;; /me and /ctcp are not defined by `defun-rcirc-command'.
(defvar rcirc-client-commands '("/me" "/ctcp")
  "A list of user commands defined by IRC client rcirc.
The list is updated automatically by `defun-rcirc-command'.")

(defun rcirc-completion-at-point ()
  "Function used for `completion-at-point-functions' in `rcirc-mode'."
  (and (rcirc-looking-at-input)
       (let* ((beg (save-excursion
		     (if (re-search-backward " " rcirc-prompt-end-marker t)
			 (1+ (point))
		       rcirc-prompt-end-marker)))
	      (table (if (and (= beg rcirc-prompt-end-marker)
			      (eq (char-after beg) ?/))
			 (delete-dups
			  (nconc (sort (copy-sequence rcirc-client-commands)
				       'string-lessp)
				 (sort (copy-sequence rcirc-server-commands)
				       'string-lessp)))
		       (rcirc-channel-nicks (rcirc-buffer-process)
					    rcirc-target))))
	 (list beg (point) table))))

(defvar rcirc-completions nil)
(defvar rcirc-completion-start nil)

(defun rcirc-complete ()
  "Cycle through completions from list of nicks in channel or IRC commands.
IRC command completion is performed only if '/' is the first input char."
  (interactive)
  (unless (rcirc-looking-at-input)
    (error "Point not located after rcirc prompt"))
  (if (eq last-command this-command)
      (setq rcirc-completions
	    (append (cdr rcirc-completions) (list (car rcirc-completions))))
    (let ((completion-ignore-case t)
	  (table (rcirc-completion-at-point)))
      (setq rcirc-completion-start (car table))
      (setq rcirc-completions
	    (and rcirc-completion-start
		 (all-completions (buffer-substring rcirc-completion-start
						    (cadr table))
				  (nth 2 table))))))
  (let ((completion (car rcirc-completions)))
    (when completion
      (delete-region rcirc-completion-start (point))
      (insert
       (cond
        ((= (aref completion 0) ?/) (concat completion " "))
        ((= rcirc-completion-start rcirc-prompt-end-marker)
         (format rcirc-nick-completion-format completion))
        (t completion))))))

(defun set-rcirc-decode-coding-system (coding-system)
  "Set the decode coding system used in this channel."
  (interactive "zCoding system for incoming messages: ")
  (set (make-local-variable 'rcirc-decode-coding-system) coding-system))

(defun set-rcirc-encode-coding-system (coding-system)
  "Set the encode coding system used in this channel."
  (interactive "zCoding system for outgoing messages: ")
  (set (make-local-variable 'rcirc-encode-coding-system) coding-system))

(defvar rcirc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'rcirc-send-input)
    (define-key map (kbd "M-p") 'rcirc-insert-prev-input)
    (define-key map (kbd "M-n") 'rcirc-insert-next-input)
    (define-key map (kbd "TAB") 'rcirc-complete)
    (define-key map (kbd "C-c C-b") 'rcirc-browse-url)
    (define-key map (kbd "C-c C-c") 'rcirc-edit-multiline)
    (define-key map (kbd "C-c C-j") 'rcirc-cmd-join)
    (define-key map (kbd "C-c C-k") 'rcirc-cmd-kick)
    (define-key map (kbd "C-c C-l") 'rcirc-toggle-low-priority)
    (define-key map (kbd "C-c C-d") 'rcirc-cmd-mode)
    (define-key map (kbd "C-c C-m") 'rcirc-cmd-msg)
    (define-key map (kbd "C-c C-r") 'rcirc-cmd-nick) ; rename
    (define-key map (kbd "C-c C-o") 'rcirc-omit-mode)
    (define-key map (kbd "C-c C-p") 'rcirc-cmd-part)
    (define-key map (kbd "C-c C-q") 'rcirc-cmd-query)
    (define-key map (kbd "C-c C-t") 'rcirc-cmd-topic)
    (define-key map (kbd "C-c C-n") 'rcirc-cmd-names)
    (define-key map (kbd "C-c C-w") 'rcirc-cmd-whois)
    (define-key map (kbd "C-c C-x") 'rcirc-cmd-quit)
    (define-key map (kbd "C-c TAB") ; C-i
      'rcirc-toggle-ignore-buffer-activity)
    (define-key map (kbd "C-c C-s") 'rcirc-switch-to-server-buffer)
    (define-key map (kbd "C-c C-a") 'rcirc-jump-to-first-unread-line)
    map)
  "Keymap for rcirc mode.")

(defvar rcirc-short-buffer-name nil
  "Generated abbreviation to use to indicate buffer activity.")

(defvar rcirc-mode-hook nil
  "Hook run when setting up rcirc buffer.")

(defvar rcirc-last-post-time nil)

(defvar rcirc-log-alist nil
  "Alist of lines to log to disk when `rcirc-log-flag' is non-nil.
Each element looks like (FILENAME . TEXT).")

(defvar rcirc-current-line 0
  "The current number of responses printed in this channel.
This number is independent of the number of lines in the buffer.")

(defun rcirc-mode (process target)
  ;; FIXME: Use define-derived-mode.
  "Major mode for IRC channel buffers.

\\{rcirc-mode-map}"
  (kill-all-local-variables)
  (use-local-map rcirc-mode-map)
  (setq mode-name "rcirc")
  (setq major-mode 'rcirc-mode)
  (setq mode-line-process nil)

  (set (make-local-variable 'rcirc-input-ring)
       ;; If rcirc-input-ring is already a ring with desired size do
       ;; not re-initialize.
       (if (and (ring-p rcirc-input-ring)
		(= (ring-size rcirc-input-ring)
		   rcirc-input-ring-size))
	   rcirc-input-ring
	 (make-ring rcirc-input-ring-size)))
  (set (make-local-variable 'rcirc-server-buffer) (process-buffer process))
  (set (make-local-variable 'rcirc-target) target)
  (set (make-local-variable 'rcirc-topic) nil)
  (set (make-local-variable 'rcirc-last-post-time) (current-time))
  (set (make-local-variable 'fill-paragraph-function) 'rcirc-fill-paragraph)
  (set (make-local-variable 'rcirc-recent-quit-alist) nil)
  (set (make-local-variable 'rcirc-current-line) 0)

  (use-hard-newlines t)
  (set (make-local-variable 'rcirc-short-buffer-name) nil)
  (set (make-local-variable 'rcirc-urls) nil)

  ;; setup for omitting responses
  (setq buffer-invisibility-spec '())
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table 4
			  (let ((glyph (make-glyph-code
					?. 'font-lock-keyword-face)))
			    (make-vector 3 glyph)))

  (dolist (i rcirc-coding-system-alist)
    (let ((chan (if (consp (car i)) (caar i) (car i)))
	  (serv (if (consp (car i)) (cdar i) "")))
      (when (and (string-match chan (or target ""))
		 (string-match serv (rcirc-server-name process)))
	(set (make-local-variable 'rcirc-decode-coding-system)
             (if (consp (cdr i)) (cadr i) (cdr i)))
        (set (make-local-variable 'rcirc-encode-coding-system)
             (if (consp (cdr i)) (cddr i) (cdr i))))))

  ;; setup the prompt and markers
  (set (make-local-variable 'rcirc-prompt-start-marker) (point-max-marker))
  (set (make-local-variable 'rcirc-prompt-end-marker) (point-max-marker))
  (rcirc-update-prompt)
  (goto-char rcirc-prompt-end-marker)

  (set (make-local-variable 'overlay-arrow-position) (make-marker))

  ;; if the user changes the major mode or kills the buffer, there is
  ;; cleanup work to do
  (add-hook 'change-major-mode-hook 'rcirc-change-major-mode-hook nil t)
  (add-hook 'kill-buffer-hook 'rcirc-kill-buffer-hook nil t)

  ;; add to buffer list, and update buffer abbrevs
  (when target				; skip server buffer
    (let ((buffer (current-buffer)))
      (with-rcirc-process-buffer process
	(setq rcirc-buffer-alist (cons (cons target buffer)
				       rcirc-buffer-alist))))
    (rcirc-update-short-buffer-names))

  (add-hook 'completion-at-point-functions
            'rcirc-completion-at-point nil 'local)

  (run-mode-hooks 'rcirc-mode-hook))

(defun rcirc-update-prompt (&optional all)
  "Reset the prompt string in the current buffer.

If ALL is non-nil, update prompts in all IRC buffers."
  (if all
      (mapc (lambda (process)
	      (mapc (lambda (buffer)
		      (with-current-buffer buffer
			(rcirc-update-prompt)))
		    (with-rcirc-process-buffer process
		      (mapcar 'cdr rcirc-buffer-alist))))
	    (rcirc-process-list))
    (let ((inhibit-read-only t)
	  (prompt (or rcirc-prompt "")))
      (mapc (lambda (rep)
	      (setq prompt
		    (replace-regexp-in-string (car rep) (cdr rep) prompt)))
	    (list (cons "%n" (rcirc-buffer-nick))
		  (cons "%s" (with-rcirc-server-buffer rcirc-server-name))
		  (cons "%t" (or rcirc-target ""))))
      (save-excursion
	(delete-region rcirc-prompt-start-marker rcirc-prompt-end-marker)
	(goto-char rcirc-prompt-start-marker)
	(let ((start (point)))
	  (insert-before-markers prompt)
	  (set-marker rcirc-prompt-start-marker start)
	  (when (not (zerop (- rcirc-prompt-end-marker
			       rcirc-prompt-start-marker)))
	    (add-text-properties rcirc-prompt-start-marker
				 rcirc-prompt-end-marker
				 (list 'face 'rcirc-prompt
				       'read-only t 'field t
				       'front-sticky t 'rear-nonsticky t))))))))

(defun rcirc-set-changed (option value)
  "Set OPTION to VALUE and do updates after a customization change."
  (set-default option value)
  (cond ((eq option 'rcirc-prompt)
	 (rcirc-update-prompt 'all))
	(t
	 (error "Bad option %s" option))))

(defun rcirc-channel-p (target)
  "Return t if TARGET is a channel name."
  (and target
       (not (zerop (length target)))
       (or (eq (aref target 0) ?#)
           (eq (aref target 0) ?&))))

(defcustom rcirc-log-directory "~/.emacs.d/rcirc-log"
  "Directory to keep IRC logfiles."
  :type 'directory
  :group 'rcirc)

(defcustom rcirc-log-flag nil
  "Non-nil means log IRC activity to disk.
Logfiles are kept in `rcirc-log-directory'."
  :type 'boolean
  :group 'rcirc)

(defun rcirc-kill-buffer-hook ()
  "Part the channel when killing an rcirc buffer."
  (when (eq major-mode 'rcirc-mode)
    (when (and rcirc-log-flag
               rcirc-log-directory)
      (rcirc-log-write))
    (rcirc-clean-up-buffer "Killed buffer")))

(defun rcirc-change-major-mode-hook ()
  "Part the channel when changing the major-mode."
  (rcirc-clean-up-buffer "Changed major mode"))

(defun rcirc-clean-up-buffer (reason)
  (let ((buffer (current-buffer)))
    (rcirc-clear-activity buffer)
    (when (and (rcirc-buffer-process)
	       (rcirc--connection-open-p (rcirc-buffer-process)))
      (with-rcirc-server-buffer
       (setq rcirc-buffer-alist
	     (rassq-delete-all buffer rcirc-buffer-alist)))
      (rcirc-update-short-buffer-names)
      (if (rcirc-channel-p rcirc-target)
	  (rcirc-send-string (rcirc-buffer-process)
			     (concat "PART " rcirc-target " :" reason))
	(when rcirc-target
	  (rcirc-remove-nick-channel (rcirc-buffer-process)
				     (rcirc-buffer-nick)
				     rcirc-target))))
    (setq rcirc-target nil)))

(defun rcirc-generate-new-buffer-name (process target)
  "Return a buffer name based on PROCESS and TARGET.
This is used for the initial name given to IRC buffers."
  (substring-no-properties
   (if target
       (concat target "@" (process-name process))
     (concat "*" (process-name process) "*"))))

(defun rcirc-get-buffer (process target &optional server)
  "Return the buffer associated with the PROCESS and TARGET.

If optional argument SERVER is non-nil, return the server buffer
if there is no existing buffer for TARGET, otherwise return nil."
  (with-rcirc-process-buffer process
    (if (null target)
	(current-buffer)
      (let ((buffer (cdr (assoc-string target rcirc-buffer-alist t))))
	(or buffer (when server (current-buffer)))))))

(defun rcirc-get-buffer-create (process target)
  "Return the buffer associated with the PROCESS and TARGET.
Create the buffer if it doesn't exist."
  (let ((buffer (rcirc-get-buffer process target)))
    (if (and buffer (buffer-live-p buffer))
	(with-current-buffer buffer
	  (when (not rcirc-target)
 	    (setq rcirc-target target))
	  buffer)
      ;; create the buffer
      (with-rcirc-process-buffer process
	(let ((new-buffer (get-buffer-create
			   (rcirc-generate-new-buffer-name process target))))
	  (with-current-buffer new-buffer
	    (rcirc-mode process target)
	    (rcirc-put-nick-channel process (rcirc-nick process) target
				    rcirc-current-line))
	  new-buffer)))))

(defun rcirc-send-input ()
  "Send input to target associated with the current buffer."
  (interactive)
  (if (< (point) rcirc-prompt-end-marker)
      ;; copy the line down to the input area
      (progn
	(forward-line 0)
	(let ((start (if (eq (point) (point-min))
			 (point)
		       (if (get-text-property (1- (point)) 'hard)
			   (point)
			 (previous-single-property-change (point) 'hard))))
	      (end (next-single-property-change (1+ (point)) 'hard)))
	  (goto-char (point-max))
	  (insert (replace-regexp-in-string
		   "\n\\s-+" " "
		   (buffer-substring-no-properties start end)))))
    ;; process input
    (goto-char (point-max))
    (when (not (equal 0 (- (point) rcirc-prompt-end-marker)))
      ;; delete a trailing newline
      (when (eq (point) (point-at-bol))
	(delete-char -1))
      (let ((input (buffer-substring-no-properties
		    rcirc-prompt-end-marker (point))))
	(dolist (line (split-string input "\n"))
	  (rcirc-process-input-line line))
	;; add to input-ring
	(save-excursion
	  (ring-insert rcirc-input-ring input)
	  (setq rcirc-input-ring-index 0))))))

(defun rcirc-fill-paragraph (&optional arg)
  (interactive "p")
  (when (> (point) rcirc-prompt-end-marker)
    (save-restriction
      (narrow-to-region rcirc-prompt-end-marker (point-max))
      (let ((fill-column rcirc-max-message-length))
	(fill-region (point-min) (point-max))))))

(defun rcirc-process-input-line (line)
  (if (string-match "^/\\([^ ]+\\) ?\\(.*\\)$" line)
      (rcirc-process-command (match-string 1 line)
			     (match-string 2 line)
			     line)
    (rcirc-process-message line)))

(defun rcirc-process-message (line)
  (if (not rcirc-target)
      (message "Not joined (no target)")
    (delete-region rcirc-prompt-end-marker (point))
    (rcirc-send-message (rcirc-buffer-process) rcirc-target line)
    (setq rcirc-last-post-time (current-time))))

(defun rcirc-process-command (command args line)
  (if (eq (aref command 0) ?/)
      ;; "//text" will send "/text" as a message
      (rcirc-process-message (substring line 1))
    (let ((fun (intern-soft (concat "rcirc-cmd-" command)))
	  (process (rcirc-buffer-process)))
      (newline)
      (with-current-buffer (current-buffer)
	(delete-region rcirc-prompt-end-marker (point))
	(if (string= command "me")
	    (rcirc-print process (rcirc-buffer-nick)
			 "ACTION" rcirc-target args)
	  (rcirc-print process (rcirc-buffer-nick)
		       "COMMAND" rcirc-target line))
	(set-marker rcirc-prompt-end-marker (point))
	(if (fboundp fun)
	    (funcall fun args process rcirc-target)
	  (rcirc-send-string process
			     (concat command " :" args)))))))

(defvar rcirc-parent-buffer nil)
(make-variable-buffer-local 'rcirc-parent-buffer)
(put 'rcirc-parent-buffer 'permanent-local t)
(defvar rcirc-window-configuration nil)
(defun rcirc-edit-multiline ()
  "Move current edit to a dedicated buffer."
  (interactive)
  (let ((pos (1+ (- (point) rcirc-prompt-end-marker))))
    (goto-char (point-max))
    (let ((text (buffer-substring-no-properties rcirc-prompt-end-marker
						(point)))
          (parent (buffer-name)))
      (delete-region rcirc-prompt-end-marker (point))
      (setq rcirc-window-configuration (current-window-configuration))
      (pop-to-buffer (concat "*multiline " parent "*"))
      (funcall rcirc-multiline-major-mode)
      (rcirc-multiline-minor-mode 1)
      (setq rcirc-parent-buffer parent)
      (insert text)
      (and (> pos 0) (goto-char pos))
      (message "Type C-c C-c to return text to %s, or C-c C-k to cancel" parent))))

(defvar rcirc-multiline-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'rcirc-multiline-minor-submit)
    (define-key map (kbd "C-x C-s") 'rcirc-multiline-minor-submit)
    (define-key map (kbd "C-c C-k") 'rcirc-multiline-minor-cancel)
    (define-key map (kbd "ESC ESC ESC") 'rcirc-multiline-minor-cancel)
    map)
  "Keymap for multiline mode in rcirc.")

(define-minor-mode rcirc-multiline-minor-mode
  "Minor mode for editing multiple lines in rcirc.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :init-value nil
  :lighter " rcirc-mline"
  :keymap rcirc-multiline-minor-mode-map
  :global nil
  :group 'rcirc
  (setq fill-column rcirc-max-message-length))

(defun rcirc-multiline-minor-submit ()
  "Send the text in buffer back to parent buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (let ((text (buffer-substring (point-min) (point-max)))
        (buffer (current-buffer))
        (pos (point)))
    (set-buffer rcirc-parent-buffer)
    (goto-char (point-max))
    (insert text)
    (kill-buffer buffer)
    (set-window-configuration rcirc-window-configuration)
    (goto-char (+ rcirc-prompt-end-marker (1- pos)))))

(defun rcirc-multiline-minor-cancel ()
  "Cancel the multiline edit."
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration rcirc-window-configuration))

(defun rcirc-any-buffer (process)
  "Return a buffer for PROCESS, either the one selected or the process buffer."
  (if rcirc-always-use-server-buffer-flag
      (process-buffer process)
    (let ((buffer (window-buffer (selected-window))))
      (if (and buffer
	       (with-current-buffer buffer
		 (and (eq major-mode 'rcirc-mode)
		      (eq (rcirc-buffer-process) process))))
	  buffer
	(process-buffer process)))))

(defcustom rcirc-response-formats
  '(("PRIVMSG" . "<%N> %m")
    ("NOTICE"  . "-%N- %m")
    ("ACTION"  . "[%N %m]")
    ("COMMAND" . "%m")
    ("ERROR"   . "%fw!!! %m")
    (t         . "%fp*** %fs%n %r %m"))
  "An alist of formats used for printing responses.
The format is looked up using the response-type as a key;
if no match is found, the default entry (with a key of `t') is used.

The entry's value part should be a string, which is inserted with
the of the following escape sequences replaced by the described values:

  %m        The message text
  %n        The sender's nick
  %N        The sender's nick (with face `rcirc-my-nick' or `rcirc-other-nick')
  %r        The response-type
  %t        The target
  %fw       Following text uses the face `font-lock-warning-face'
  %fp       Following text uses the face `rcirc-server-prefix'
  %fs       Following text uses the face `rcirc-server'
  %f[FACE]  Following text uses the face FACE
  %f-       Following text uses the default face
  %%        A literal `%' character"
  :type '(alist :key-type (choice (string :tag "Type")
				  (const :tag "Default" t))
		:value-type string)
  :group 'rcirc)

(defcustom rcirc-omit-responses
  '("JOIN" "PART" "QUIT" "NICK")
  "Responses which will be hidden when `rcirc-omit-mode' is enabled."
  :type '(repeat string)
  :group 'rcirc)

(defun rcirc-format-response-string (process sender response target text)
  "Return a nicely-formatted response string, incorporating TEXT
\(and perhaps other arguments).  The specific formatting used
is found by looking up RESPONSE in `rcirc-response-formats'."
  (with-temp-buffer
    (insert (or (cdr (assoc response rcirc-response-formats))
		(cdr (assq t rcirc-response-formats))))
    (goto-char (point-min))
    (let ((start (point-min))
	  (sender (if (or (not sender)
			  (string= (rcirc-server-name process) sender))
		      ""
		    sender))
	  face)
      (while (re-search-forward "%\\(\\(f\\(.\\)\\)\\|\\(.\\)\\)" nil t)
	(rcirc-add-face start (match-beginning 0) face)
	(setq start (match-beginning 0))
	(replace-match
	 (case (aref (match-string 1) 0)
	    (?f (setq face
		      (case (string-to-char (match-string 3))
			(?w 'font-lock-warning-face)
			(?p 'rcirc-server-prefix)
			(?s 'rcirc-server)
			(t nil)))
		"")
	    (?n sender)
	    (?N (let ((my-nick (rcirc-nick process)))
		  (save-match-data
		    (with-syntax-table rcirc-nick-syntax-table
		      (rcirc-facify sender
				    (cond ((string= sender my-nick)
					   'rcirc-my-nick)
					  ((and rcirc-bright-nicks
						(string-match
						 (regexp-opt rcirc-bright-nicks
							     'words)
						 sender))
					   'rcirc-bright-nick)
					  ((and rcirc-dim-nicks
						(string-match
						 (regexp-opt rcirc-dim-nicks
							     'words)
						 sender))
					   'rcirc-dim-nick)
					  (t
					   'rcirc-other-nick)))))))
	    (?m (propertize text 'rcirc-text text))
	    (?r response)
	    (?t (or target ""))
	    (t (concat "UNKNOWN CODE:" (match-string 0))))
	 t t nil 0)
	(rcirc-add-face (match-beginning 0) (match-end 0) face))
      (rcirc-add-face start (match-beginning 0) face))
      (buffer-substring (point-min) (point-max))))

(defun rcirc-target-buffer (process sender response target text)
  "Return a buffer to print the server response."
  (assert (not (bufferp target)))
  (with-rcirc-process-buffer process
    (cond ((not target)
	   (rcirc-any-buffer process))
	  ((not (rcirc-channel-p target))
	   ;; message from another user
	   (if (or (string= response "PRIVMSG")
		   (string= response "ACTION"))
	       (rcirc-get-buffer-create process (if (string= sender rcirc-nick)
						    target
						  sender))
	     (rcirc-get-buffer process target t)))
	  ((or (rcirc-get-buffer process target)
	       (rcirc-any-buffer process))))))

(defvar rcirc-activity-types nil)
(make-variable-buffer-local 'rcirc-activity-types)
(defvar rcirc-last-sender nil)
(make-variable-buffer-local 'rcirc-last-sender)

(defcustom rcirc-omit-threshold 100
  "Number of lines since last activity from a nick before `rcirc-omit-responses' are omitted."
  :type 'integer
  :group 'rcirc)

(defcustom rcirc-log-process-buffers nil
  "Non-nil if rcirc process buffers should be logged to disk."
  :group 'rcirc
  :type 'boolean
  :version "24.1")

(defun rcirc-last-quit-line (process nick target)
  "Return the line number where NICK left TARGET.
Returns nil if the information is not recorded."
  (let ((chanbuf (rcirc-get-buffer process target)))
    (when chanbuf
      (cdr (assoc-string nick (with-current-buffer chanbuf
				rcirc-recent-quit-alist))))))

(defun rcirc-last-line (process nick target)
  "Return the line from the last activity from NICK in TARGET."
  (let* ((chanbuf (rcirc-get-buffer process target))
	 (line (or (cdr (assoc-string target
				      (gethash nick (with-rcirc-server-buffer
						      rcirc-nick-table)) t))
		   (rcirc-last-quit-line process nick target))))
    (if line
	line
      ;;(message "line is nil for %s in %s" nick target)
      nil)))

(defun rcirc-elapsed-lines (process nick target)
  "Return the number of lines since activity from NICK in TARGET."
  (let ((last-activity-line (rcirc-last-line process nick target)))
    (when (and last-activity-line
	       (> last-activity-line 0))
      (- rcirc-current-line last-activity-line))))

(defvar rcirc-markup-text-functions
  '(rcirc-markup-attributes
    rcirc-markup-my-nick
    rcirc-markup-urls
    rcirc-markup-keywords
    rcirc-markup-bright-nicks)

  "List of functions used to manipulate text before it is printed.

Each function takes two arguments, SENDER, and RESPONSE.  The
buffer is narrowed with the text to be printed and the point is
at the beginning of the `rcirc-text' propertized text.")

(defun rcirc-print (process sender response target text &optional activity)
  "Print TEXT in the buffer associated with TARGET.
Format based on SENDER and RESPONSE.  If ACTIVITY is non-nil,
record activity."
  (or text (setq text ""))
  (unless (and (or (member sender rcirc-ignore-list)
		   (member (with-syntax-table rcirc-nick-syntax-table
			     (when (string-match "^\\([^/]\\w*\\)[:,]" text)
			       (match-string 1 text)))
			   rcirc-ignore-list))
	       ;; do not ignore if we sent the message
 	       (not (string= sender (rcirc-nick process))))
    (let* ((buffer (rcirc-target-buffer process sender response target text))
	   (inhibit-read-only t))
      (with-current-buffer buffer
	(let ((moving (= (point) rcirc-prompt-end-marker))
	      (old-point (point-marker))
	      (fill-start (marker-position rcirc-prompt-start-marker)))

	  (setq text (decode-coding-string text rcirc-decode-coding-system))
	  (unless (string= sender (rcirc-nick process))
	    ;; mark the line with overlay arrow
	    (unless (or (marker-position overlay-arrow-position)
			(get-buffer-window (current-buffer))
			(member response rcirc-omit-responses))
	      (set-marker overlay-arrow-position
			  (marker-position rcirc-prompt-start-marker))))

	  ;; temporarily set the marker insertion-type because
	  ;; insert-before-markers results in hidden text in new buffers
	  (goto-char rcirc-prompt-start-marker)
	  (set-marker-insertion-type rcirc-prompt-start-marker t)
	  (set-marker-insertion-type rcirc-prompt-end-marker t)

	  (let ((start (point)))
	    (insert (rcirc-format-response-string process sender response nil
						  text)
		    (propertize "\n" 'hard t))

 	    ;; squeeze spaces out of text before rcirc-text
	    (fill-region fill-start
			 (1- (or (next-single-property-change fill-start
							      'rcirc-text)
				 rcirc-prompt-end-marker)))

	    ;; run markup functions
 	    (save-excursion
 	      (save-restriction
 		(narrow-to-region start rcirc-prompt-start-marker)
		(goto-char (or (next-single-property-change start 'rcirc-text)
			       (point)))
		(when (rcirc-buffer-process)
		  (save-excursion (rcirc-markup-timestamp sender response))
		  (dolist (fn rcirc-markup-text-functions)
		    (save-excursion (funcall fn sender response)))
		  (when rcirc-fill-flag
		    (save-excursion (rcirc-markup-fill sender response))))

		(when rcirc-read-only-flag
		  (add-text-properties (point-min) (point-max)
				       '(read-only t front-sticky t))))
	      ;; make text omittable
	      (let ((last-activity-lines (rcirc-elapsed-lines process sender target)))
		(if (and (not (string= (rcirc-nick process) sender))
			 (member response rcirc-omit-responses)
			 (or (not last-activity-lines)
			     (< rcirc-omit-threshold last-activity-lines)))
		    (put-text-property (1- start) (1- rcirc-prompt-start-marker)
				       'invisible 'rcirc-omit)
		  ;; otherwise increment the line count
		  (setq rcirc-current-line (1+ rcirc-current-line))))))

	  (set-marker-insertion-type rcirc-prompt-start-marker nil)
	  (set-marker-insertion-type rcirc-prompt-end-marker nil)

	  ;; truncate buffer if it is very long
	  (save-excursion
	    (when (and rcirc-buffer-maximum-lines
		       (> rcirc-buffer-maximum-lines 0)
		       (= (forward-line (- rcirc-buffer-maximum-lines)) 0))
	      (delete-region (point-min) (point))))

	  ;; set the window point for buffers show in windows
	  (walk-windows (lambda (w)
			  (when (and (not (eq (selected-window) w))
				     (eq (current-buffer)
					 (window-buffer w))
				     (>= (window-point w)
					 rcirc-prompt-end-marker))
			      (set-window-point w (point-max))))
			nil t)

	  ;; restore the point
	  (goto-char (if moving rcirc-prompt-end-marker old-point))

	  ;; keep window on bottom line if it was already there
	  (when rcirc-scroll-show-maximum-output
	    (let ((window (get-buffer-window)))
	      (when window
		(with-selected-window window
		  (when (eq major-mode 'rcirc-mode)
		    (when (<= (- (window-height)
				 (count-screen-lines (window-point)
						     (window-start))
				 1)
			      0)
		      (recenter -1)))))))

	  ;; flush undo (can we do something smarter here?)
	  (buffer-disable-undo)
	  (buffer-enable-undo))

	;; record modeline activity
	(when (and activity
		   (not rcirc-ignore-buffer-activity-flag)
		   (not (and rcirc-dim-nicks sender
			     (string-match (regexp-opt rcirc-dim-nicks) sender)
			     (rcirc-channel-p target))))
	      (rcirc-record-activity (current-buffer)
				     (when (not (rcirc-channel-p rcirc-target))
				       'nick)))

	(when (and rcirc-log-flag
		   (or target
		       rcirc-log-process-buffers))
	  (rcirc-log process sender response target text))

	(sit-for 0)			; displayed text before hook
	(run-hook-with-args 'rcirc-print-hooks
			    process sender response target text)))))

(defun rcirc-generate-log-filename (process target)
  (if target
      (rcirc-generate-new-buffer-name process target)
    (process-name process)))

(defcustom rcirc-log-filename-function 'rcirc-generate-log-filename
  "A function to generate the filename used by rcirc's logging facility.

It is called with two arguments, PROCESS and TARGET (see
`rcirc-generate-new-buffer-name' for their meaning), and should
return the filename, or nil if no logging is desired for this
session.

If the returned filename is absolute (`file-name-absolute-p'
returns t), then it is used as-is, otherwise the resulting file
is put into `rcirc-log-directory'.

The filename is then cleaned using `convert-standard-filename' to
guarantee valid filenames for the current OS."
  :group 'rcirc
  :type 'function)

(defun rcirc-log (process sender response target text)
  "Record line in `rcirc-log', to be later written to disk."
  (let ((filename (funcall rcirc-log-filename-function process target)))
    (unless (null filename)
      (let ((cell (assoc-string filename rcirc-log-alist))
	    (line (concat (format-time-string rcirc-time-format)
			  (substring-no-properties
			   (rcirc-format-response-string process sender
							 response target text))
			  "\n")))
	(if cell
	    (setcdr cell (concat (cdr cell) line))
	  (setq rcirc-log-alist
		(cons (cons filename line) rcirc-log-alist)))))))

(defun rcirc-log-write ()
  "Flush `rcirc-log-alist' data to disk.

Log data is written to `rcirc-log-directory', except for
log-files with absolute names (see `rcirc-log-filename-function')."
  (dolist (cell rcirc-log-alist)
    (let ((filename (convert-standard-filename
                     (expand-file-name (car cell)
                                       rcirc-log-directory)))
	  (coding-system-for-write 'utf-8))
      (make-directory (file-name-directory filename) t)
      (with-temp-buffer
	(insert (cdr cell))
	(write-region (point-min) (point-max) filename t 'quiet))))
  (setq rcirc-log-alist nil))

(defun rcirc-view-log-file ()
  "View logfile corresponding to the current buffer."
  (interactive)
  (find-file-other-window
   (expand-file-name (funcall rcirc-log-filename-function
			      (rcirc-buffer-process) rcirc-target)
		     rcirc-log-directory)))

(defun rcirc-join-channels (process channels)
  "Join CHANNELS."
  (save-window-excursion
    (dolist (channel channels)
      (with-rcirc-process-buffer process
	(rcirc-cmd-join channel process)))))

;;; nick management
(defvar rcirc-nick-prefix-chars "~&@%+")
(defun rcirc-user-nick (user)
  "Return the nick from USER.  Remove any non-nick junk."
  (save-match-data
    (if (string-match (concat "^[" rcirc-nick-prefix-chars
			      "]?\\([^! ]+\\)!?") (or user ""))
	(match-string 1 user)
      user)))

(defun rcirc-nick-channels (process nick)
  "Return list of channels for NICK."
  (with-rcirc-process-buffer process
    (mapcar (lambda (x) (car x))
	    (gethash nick rcirc-nick-table))))

(defun rcirc-put-nick-channel (process nick channel &optional line)
  "Add CHANNEL to list associated with NICK.
Update the associated linestamp if LINE is non-nil.

If the record doesn't exist, and LINE is nil, set the linestamp
to zero."
  (let ((nick (rcirc-user-nick nick)))
    (with-rcirc-process-buffer process
      (let* ((chans (gethash nick rcirc-nick-table))
	     (record (assoc-string channel chans t)))
	(if record
	    (when line (setcdr record line))
	  (puthash nick (cons (cons channel (or line 0))
			      chans)
		   rcirc-nick-table))))))

(defun rcirc-nick-remove (process nick)
  "Remove NICK from table."
  (with-rcirc-process-buffer process
    (remhash nick rcirc-nick-table)))

(defun rcirc-remove-nick-channel (process nick channel)
  "Remove the CHANNEL from list associated with NICK."
  (with-rcirc-process-buffer process
    (let* ((chans (gethash nick rcirc-nick-table))
           (newchans
	    ;; instead of assoc-string-delete-all:
	    (let ((record (assoc-string channel chans t)))
	      (when record
		(setcar record 'delete)
		(assq-delete-all 'delete chans)))))
      (if newchans
          (puthash nick newchans rcirc-nick-table)
        (remhash nick rcirc-nick-table)))))

(defun rcirc-channel-nicks (process target)
  "Return the list of nicks associated with TARGET sorted by last activity."
  (when target
    (if (rcirc-channel-p target)
	(with-rcirc-process-buffer process
	  (let (nicks)
	    (maphash
	     (lambda (k v)
	       (let ((record (assoc-string target v t)))
		 (if record
		     (setq nicks (cons (cons k (cdr record)) nicks)))))
	     rcirc-nick-table)
	    (mapcar (lambda (x) (car x))
		    (sort nicks (lambda (x y)
				  (let ((lx (or (cdr x) 0))
					(ly (or (cdr y) 0)))
				    (< ly lx)))))))
      (list target))))

(defun rcirc-ignore-update-automatic (nick)
  "Remove NICK from `rcirc-ignore-list'
if NICK is also on `rcirc-ignore-list-automatic'."
  (when (member nick rcirc-ignore-list-automatic)
      (setq rcirc-ignore-list-automatic
	    (delete nick rcirc-ignore-list-automatic)
	    rcirc-ignore-list
	    (delete nick rcirc-ignore-list))))

(defun rcirc-nickname< (s1 s2)
  "Return t if IRC nickname S1 is less than S2, and nil otherwise.
Operator nicknames (@) are considered less than voiced
nicknames (+).  Any other nicknames are greater than voiced
nicknames.  The comparison is case-insensitive."
  (setq s1 (downcase s1)
        s2 (downcase s2))
  (let* ((s1-op (eq ?@ (string-to-char s1)))
         (s2-op (eq ?@ (string-to-char s2))))
    (if s1-op
        (if s2-op
            (string< (substring s1 1) (substring s2 1))
          t)
      (if s2-op
          nil
        (string< s1 s2)))))

(defun rcirc-sort-nicknames-join (input sep)
  "Return a string of sorted nicknames.
INPUT is a string containing nicknames separated by SEP.
This function does not alter the INPUT string."
  (let* ((parts (split-string input sep t))
         (sorted (sort parts 'rcirc-nickname<)))
    (mapconcat 'identity sorted sep)))

;;; activity tracking
(defvar rcirc-track-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-@") 'rcirc-next-active-buffer)
    (define-key map (kbd "C-c C-SPC") 'rcirc-next-active-buffer)
    map)
  "Keymap for rcirc track minor mode.")

;;;###autoload
(define-minor-mode rcirc-track-minor-mode
  "Global minor mode for tracking activity in rcirc buffers.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :init-value nil
  :lighter ""
  :keymap rcirc-track-minor-mode-map
  :global t
  :group 'rcirc
  (or global-mode-string (setq global-mode-string '("")))
  ;; toggle the mode-line channel indicator
  (if rcirc-track-minor-mode
      (progn
	(and (not (memq 'rcirc-activity-string global-mode-string))
	     (setq global-mode-string
		   (append global-mode-string '(rcirc-activity-string))))
	(add-hook 'window-configuration-change-hook
		  'rcirc-window-configuration-change))
    (setq global-mode-string
	  (delete 'rcirc-activity-string global-mode-string))
    (remove-hook 'window-configuration-change-hook
		 'rcirc-window-configuration-change)))

(or (assq 'rcirc-ignore-buffer-activity-flag minor-mode-alist)
    (setq minor-mode-alist
          (cons '(rcirc-ignore-buffer-activity-flag " Ignore") minor-mode-alist)))
(or (assq 'rcirc-low-priority-flag minor-mode-alist)
    (setq minor-mode-alist
          (cons '(rcirc-low-priority-flag " LowPri") minor-mode-alist)))
(or (assq 'rcirc-omit-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(rcirc-omit-mode " Omit") minor-mode-alist)))

(defun rcirc-toggle-ignore-buffer-activity ()
  "Toggle the value of `rcirc-ignore-buffer-activity-flag'."
  (interactive)
  (setq rcirc-ignore-buffer-activity-flag
	(not rcirc-ignore-buffer-activity-flag))
  (message (if rcirc-ignore-buffer-activity-flag
	       "Ignore activity in this buffer"
	     "Notice activity in this buffer"))
  (force-mode-line-update))

(defun rcirc-toggle-low-priority ()
  "Toggle the value of `rcirc-low-priority-flag'."
  (interactive)
  (setq rcirc-low-priority-flag
	(not rcirc-low-priority-flag))
  (message (if rcirc-low-priority-flag
	       "Activity in this buffer is low priority"
	     "Activity in this buffer is normal priority"))
  (force-mode-line-update))

(defun rcirc-omit-mode ()
  "Toggle the Rcirc-Omit mode.
If enabled, \"uninteresting\" lines are not shown.
Uninteresting lines are those whose responses are listed in
`rcirc-omit-responses'."
  (interactive)
  (setq rcirc-omit-mode (not rcirc-omit-mode))
  (if rcirc-omit-mode
      (progn
	(add-to-invisibility-spec '(rcirc-omit . nil))
	(message "Rcirc-Omit mode enabled"))
    (remove-from-invisibility-spec '(rcirc-omit . nil))
    (message "Rcirc-Omit mode disabled"))
    (recenter (when (> (point) rcirc-prompt-start-marker) -1)))

(defun rcirc-switch-to-server-buffer ()
  "Switch to the server buffer associated with current channel buffer."
  (interactive)
  (unless (buffer-live-p rcirc-server-buffer)
    (error "No such buffer"))
  (switch-to-buffer rcirc-server-buffer))

(defun rcirc-jump-to-first-unread-line ()
  "Move the point to the first unread line in this buffer."
  (interactive)
  (if (marker-position overlay-arrow-position)
      (goto-char overlay-arrow-position)
    (message "No unread messages")))

(defun rcirc-non-irc-buffer ()
  (let ((buflist (buffer-list))
	buffer)
    (while (and buflist (not buffer))
      (with-current-buffer (car buflist)
	(unless (or (eq major-mode 'rcirc-mode)
		    (= ?\s (aref (buffer-name) 0)) ; internal buffers
		    (get-buffer-window (current-buffer)))
	  (setq buffer (current-buffer))))
      (setq buflist (cdr buflist)))
    buffer))

(defun rcirc-next-active-buffer (arg)
  "Switch to the next rcirc buffer with activity.
With prefix ARG, go to the next low priority buffer with activity."
  (interactive "P")
  (let* ((pair (rcirc-split-activity rcirc-activity))
	 (lopri (car pair))
	 (hipri (cdr pair)))
    (if (or (and (not arg) hipri)
	    (and arg lopri))
	(progn
	  (switch-to-buffer (car (if arg lopri hipri)))
	  (when (> (point) rcirc-prompt-start-marker)
	    (recenter -1)))
      (if (eq major-mode 'rcirc-mode)
	  (switch-to-buffer (rcirc-non-irc-buffer))
	(message "%s" (concat
		       "No IRC activity."
		       (when lopri
			 (concat
			  "  Type C-u "
			  (key-description (this-command-keys))
			  " for low priority activity."))))))))

(defvar rcirc-activity-hooks nil
  "Hook to be run when there is channel activity.

Functions are called with a single argument, the buffer with the
activity.  Only run if the buffer is not visible and
`rcirc-ignore-buffer-activity-flag' is non-nil.")

(defun rcirc-record-activity (buffer &optional type)
  "Record BUFFER activity with TYPE."
  (with-current-buffer buffer
    (let ((old-activity rcirc-activity)
	  (old-types rcirc-activity-types))
      (when (not (get-buffer-window (current-buffer) t))
	(setq rcirc-activity
	      (sort (add-to-list 'rcirc-activity (current-buffer))
		    (lambda (b1 b2)
		      (let ((t1 (with-current-buffer b1 rcirc-last-post-time))
			    (t2 (with-current-buffer b2 rcirc-last-post-time)))
			(time-less-p t2 t1)))))
	(pushnew type rcirc-activity-types)
	(unless (and (equal rcirc-activity old-activity)
		     (member type old-types))
	  (rcirc-update-activity-string)))))
  (run-hook-with-args 'rcirc-activity-hooks buffer))

(defun rcirc-clear-activity (buffer)
  "Clear the BUFFER activity."
  (setq rcirc-activity (remove buffer rcirc-activity))
  (with-current-buffer buffer
    (setq rcirc-activity-types nil)))

(defun rcirc-clear-unread (buffer)
  "Erase the last read message arrow from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-marker overlay-arrow-position nil))))

(defun rcirc-split-activity (activity)
  "Return a cons cell with ACTIVITY split into (lopri . hipri)."
  (let (lopri hipri)
    (dolist (buf rcirc-activity)
      (with-current-buffer buf
	(if (and rcirc-low-priority-flag
		 (not (member 'nick rcirc-activity-types)))
	    (add-to-list 'lopri buf t)
	  (add-to-list 'hipri buf t))))
    (cons lopri hipri)))

(defvar rcirc-update-activity-string-hook nil
  "Hook run whenever the activity string is updated.")

;; TODO: add mouse properties
(defun rcirc-update-activity-string ()
  "Update mode-line string."
  (let* ((pair (rcirc-split-activity rcirc-activity))
	 (lopri (car pair))
	 (hipri (cdr pair)))
    (setq rcirc-activity-string
	  (cond ((or hipri lopri)
		 (concat (and hipri "[")
			 (rcirc-activity-string hipri)
			 (and hipri lopri ",")
			 (and lopri
			      (concat "("
				      (rcirc-activity-string lopri)
				      ")"))
			 (and hipri "]")))
		((not (null (rcirc-process-list)))
		 "[]")
		(t "[]")))
    (run-hooks 'rcirc-update-activity-string-hook)))

(defun rcirc-activity-string (buffers)
  (mapconcat (lambda (b)
	       (let ((s (substring-no-properties (rcirc-short-buffer-name b))))
		 (with-current-buffer b
		   (dolist (type rcirc-activity-types)
		     (rcirc-add-face 0 (length s)
				     (case type
				       (nick 'rcirc-track-nick)
				       (keyword 'rcirc-track-keyword))
				     s)))
		 s))
	     buffers ","))

(defun rcirc-short-buffer-name (buffer)
  "Return a short name for BUFFER to use in the modeline indicator."
  (with-current-buffer buffer
    (or rcirc-short-buffer-name (buffer-name))))

(defun rcirc-visible-buffers ()
  "Return a list of the visible buffers that are in rcirc-mode."
  (let (acc)
    (walk-windows (lambda (w)
		    (with-current-buffer (window-buffer w)
		      (when (eq major-mode 'rcirc-mode)
			(push (current-buffer) acc)))))
    acc))

(defvar rcirc-visible-buffers nil)
(defun rcirc-window-configuration-change ()
  (unless (minibuffer-window-active-p (minibuffer-window))
    ;; delay this until command has finished to make sure window is
    ;; actually visible before clearing activity
    (add-hook 'post-command-hook 'rcirc-window-configuration-change-1)))

(defun rcirc-window-configuration-change-1 ()
  ;; clear activity and overlay arrows
  (let* ((old-activity rcirc-activity)
	 (hidden-buffers rcirc-visible-buffers))

    (setq rcirc-visible-buffers (rcirc-visible-buffers))

    (dolist (vbuf rcirc-visible-buffers)
      (setq hidden-buffers (delq vbuf hidden-buffers))
      ;; clear activity for all visible buffers
      (rcirc-clear-activity vbuf))

    ;; clear unread arrow from recently hidden buffers
    (dolist (hbuf hidden-buffers)
      (rcirc-clear-unread hbuf))

    ;; remove any killed buffers from list
    (setq rcirc-activity
	  (delq nil (mapcar (lambda (buf) (when (buffer-live-p buf) buf))
			    rcirc-activity)))
    ;; update the mode-line string
    (unless (equal old-activity rcirc-activity)
      (rcirc-update-activity-string)))

  (remove-hook 'post-command-hook 'rcirc-window-configuration-change-1))


;;; buffer name abbreviation
(defun rcirc-update-short-buffer-names ()
  (let ((bufalist
	 (apply 'append (mapcar (lambda (process)
				  (with-rcirc-process-buffer process
				    rcirc-buffer-alist))
				(rcirc-process-list)))))
    (dolist (i (rcirc-abbreviate bufalist))
      (when (buffer-live-p (cdr i))
	(with-current-buffer (cdr i)
	  (setq rcirc-short-buffer-name (car i)))))))

(defun rcirc-abbreviate (pairs)
  (apply 'append (mapcar 'rcirc-rebuild-tree (rcirc-make-trees pairs))))

(defun rcirc-rebuild-tree (tree &optional acc)
  (let ((ch (char-to-string (car tree))))
    (dolist (x (cdr tree))
      (if (listp x)
	  (setq acc (append acc
			   (mapcar (lambda (y)
				     (cons (concat ch (car y))
					   (cdr y)))
				   (rcirc-rebuild-tree x))))
	(setq acc (cons (cons ch x) acc))))
    acc))

(defun rcirc-make-trees (pairs)
  (let (alist)
    (mapc (lambda (pair)
	    (if (consp pair)
		(let* ((str (car pair))
		       (data (cdr pair))
		       (char (unless (zerop (length str))
			       (aref str 0)))
		       (rest (unless (zerop (length str))
			       (substring str 1)))
		       (part (if char (assq char alist))))
		  (if part
		      ;; existing partition
		      (setcdr part (cons (cons rest data) (cdr part)))
		    ;; new partition
		    (setq alist (cons (if char
					  (list char (cons rest data))
					data)
				      alist))))
	      (setq alist (cons pair alist))))
	  pairs)
    ;; recurse into cdrs of alist
    (mapc (lambda (x)
	    (when (and (listp x) (listp (cadr x)))
	      (setcdr x (if (> (length (cdr x)) 1)
			    (rcirc-make-trees (cdr x))
			  (setcdr x (list (cdadr x)))))))
	  alist)))

;;; /commands these are called with 3 args: PROCESS, TARGET, which is
;; the current buffer/channel/user, and ARGS, which is a string
;; containing the text following the /cmd.

(defmacro defun-rcirc-command (command argument docstring interactive-form
				       &rest body)
  "Define a command."
  `(progn
     (add-to-list 'rcirc-client-commands ,(concat "/" (symbol-name command)))
     (defun ,(intern (concat "rcirc-cmd-" (symbol-name command)))
       (,@argument &optional process target)
       ,(concat docstring "\n\nNote: If PROCESS or TARGET are nil, the values given"
		"\nby `rcirc-buffer-process' and `rcirc-target' will be used.")
       ,interactive-form
       (let ((process (or process (rcirc-buffer-process)))
	     (target (or target rcirc-target)))
	 ,@body))))

(defun-rcirc-command msg (message)
  "Send private MESSAGE to TARGET."
  (interactive "i")
  (if (null message)
      (progn
        (setq target (completing-read "Message nick: "
                                      (with-rcirc-server-buffer
					rcirc-nick-table)))
        (when (> (length target) 0)
          (setq message (read-string (format "Message %s: " target)))
          (when (> (length message) 0)
            (rcirc-send-message process target message))))
    (if (not (string-match "\\([^ ]+\\) \\(.+\\)" message))
        (message "Not enough args, or something.")
      (setq target (match-string 1 message)
            message (match-string 2 message))
      (rcirc-send-message process target message))))

(defun-rcirc-command query (nick)
  "Open a private chat buffer to NICK."
  (interactive (list (completing-read "Query nick: "
                                      (with-rcirc-server-buffer rcirc-nick-table))))
  (let ((existing-buffer (rcirc-get-buffer process nick)))
    (switch-to-buffer (or existing-buffer
			  (rcirc-get-buffer-create process nick)))
    (when (not existing-buffer)
      (rcirc-cmd-whois nick))))

(defun-rcirc-command join (channels)
  "Join CHANNELS.
CHANNELS is a comma- or space-separated string of channel names."
  (interactive "sJoin channels: ")
  (let* ((split-channels (split-string channels "[ ,]" t))
         (buffers (mapcar (lambda (ch)
                            (rcirc-get-buffer-create process ch))
                          split-channels))
         (channels (mapconcat 'identity split-channels ",")))
    (rcirc-send-string process (concat "JOIN " channels))
    (when (not (eq (selected-window) (minibuffer-window)))
      (dolist (b buffers) ;; order the new channel buffers in the buffer list
        (switch-to-buffer b)))))

(defun-rcirc-command invite (nick-channel)
  "Invite NICK to CHANNEL."
  (interactive (list
		(concat
		 (completing-read "Invite nick: "
				  (with-rcirc-server-buffer rcirc-nick-table))
		 " "
		 (read-string "Channel: "))))
  (rcirc-send-string process (concat "INVITE " nick-channel)))

;; TODO: /part #channel reason, or consider removing #channel altogether
(defun-rcirc-command part (channel)
  "Part CHANNEL."
  (interactive "sPart channel: ")
  (let ((channel (if (> (length channel) 0) channel target)))
    (rcirc-send-string process (concat "PART " channel " :" rcirc-id-string))))

(defun-rcirc-command quit (reason)
  "Send a quit message to server with REASON."
  (interactive "sQuit reason: ")
  (rcirc-send-string process (concat "QUIT :"
				     (if (not (zerop (length reason)))
					 reason
				       rcirc-id-string))))

(defun-rcirc-command nick (nick)
  "Change nick to NICK."
  (interactive "i")
  (when (null nick)
    (setq nick (read-string "New nick: " (rcirc-nick process))))
  (rcirc-send-string process (concat "NICK " nick)))

(defun-rcirc-command names (channel)
  "Display list of names in CHANNEL or in current channel if CHANNEL is nil.
If called interactively, prompt for a channel when prefix arg is supplied."
  (interactive "P")
  (if (called-interactively-p 'interactive)
      (if channel
          (setq channel (read-string "List names in channel: " target))))
  (let ((channel (if (> (length channel) 0)
                     channel
                   target)))
    (rcirc-send-string process (concat "NAMES " channel))))

(defun-rcirc-command topic (topic)
  "List TOPIC for the TARGET channel.
With a prefix arg, prompt for new topic."
  (interactive "P")
  (if (and (called-interactively-p 'interactive) topic)
      (setq topic (read-string "New Topic: " rcirc-topic)))
  (rcirc-send-string process (concat "TOPIC " target
                                     (when (> (length topic) 0)
                                       (concat " :" topic)))))

(defun-rcirc-command whois (nick)
  "Request information from server about NICK."
  (interactive (list
                (completing-read "Whois: "
                                 (with-rcirc-server-buffer rcirc-nick-table))))
  (rcirc-send-string process (concat "WHOIS " nick)))

(defun-rcirc-command mode (args)
  "Set mode with ARGS."
  (interactive (list (concat (read-string "Mode nick or channel: ")
                             " " (read-string "Mode: "))))
  (rcirc-send-string process (concat "MODE " args)))

(defun-rcirc-command list (channels)
  "Request information on CHANNELS from server."
  (interactive "sList Channels: ")
  (rcirc-send-string process (concat "LIST " channels)))

(defun-rcirc-command oper (args)
  "Send operator command to server."
  (interactive "sOper args: ")
  (rcirc-send-string process (concat "OPER " args)))

(defun-rcirc-command quote (message)
  "Send MESSAGE literally to server."
  (interactive "sServer message: ")
  (rcirc-send-string process message))

(defun-rcirc-command kick (arg)
  "Kick NICK from current channel."
  (interactive (list
                (concat (completing-read "Kick nick: "
                                         (rcirc-channel-nicks
					  (rcirc-buffer-process)
					  rcirc-target))
                        (read-from-minibuffer "Kick reason: "))))
  (let* ((arglist (split-string arg))
         (argstring (concat (car arglist) " :"
                            (mapconcat 'identity (cdr arglist) " "))))
    (rcirc-send-string process (concat "KICK " target " " argstring))))

(defun rcirc-cmd-ctcp (args &optional process target)
  (if (string-match "^\\([^ ]+\\)\\s-+\\(.+\\)$" args)
      (let* ((target (match-string 1 args))
             (request (upcase (match-string 2 args)))
             (function (intern-soft (concat "rcirc-ctcp-sender-" request))))
        (if (fboundp function) ;; use special function if available
            (funcall function process target request)
          (rcirc-send-ctcp process target request)))
    (rcirc-print process (rcirc-nick process) "ERROR" nil
                 "usage: /ctcp NICK REQUEST")))

(defun rcirc-ctcp-sender-PING (process target request)
  "Send a CTCP PING message to TARGET."
  (let ((timestamp (format "%.0f" (rcirc-float-time))))
    (rcirc-send-ctcp process target "PING" timestamp)))

(defun rcirc-cmd-me (args &optional process target)
  (rcirc-send-ctcp process target "ACTION" args))

(defun rcirc-add-or-remove (set &rest elements)
  (dolist (elt elements)
    (if (and elt (not (string= "" elt)))
	(setq set (if (member-ignore-case elt set)
		      (delete elt set)
		    (cons elt set)))))
  set)

(defun-rcirc-command ignore (nick)
  "Manage the ignore list.
Ignore NICK, unignore NICK if already ignored, or list ignored
nicks when no NICK is given.  When listing ignored nicks, the
ones added to the list automatically are marked with an asterisk."
  (interactive "sToggle ignoring of nick: ")
  (setq rcirc-ignore-list
	(apply #'rcirc-add-or-remove rcirc-ignore-list
	       (split-string nick nil t)))
  (rcirc-print process nil "IGNORE" target
	       (mapconcat
		(lambda (nick)
		  (concat nick
			  (if (member nick rcirc-ignore-list-automatic)
			      "*" "")))
		rcirc-ignore-list " ")))

(defun-rcirc-command bright (nick)
  "Manage the bright nick list."
  (interactive "sToggle emphasis of nick: ")
  (setq rcirc-bright-nicks
	(apply #'rcirc-add-or-remove rcirc-bright-nicks
	       (split-string nick nil t)))
  (rcirc-print process nil "BRIGHT" target
	       (mapconcat 'identity rcirc-bright-nicks " ")))

(defun-rcirc-command dim (nick)
  "Manage the dim nick list."
  (interactive "sToggle deemphasis of nick: ")
  (setq rcirc-dim-nicks
	(apply #'rcirc-add-or-remove rcirc-dim-nicks
	       (split-string nick nil t)))
  (rcirc-print process nil "DIM" target
	       (mapconcat 'identity rcirc-dim-nicks " ")))

(defun-rcirc-command keyword (keyword)
  "Manage the keyword list.
Mark KEYWORD, unmark KEYWORD if already marked, or list marked
keywords when no KEYWORD is given."
  (interactive "sToggle highlighting of keyword: ")
  (setq rcirc-keywords
	(apply #'rcirc-add-or-remove rcirc-keywords
	       (split-string keyword nil t)))
  (rcirc-print process nil "KEYWORD" target
	       (mapconcat 'identity rcirc-keywords " ")))


(defun rcirc-add-face (start end name &optional object)
  "Add face NAME to the face text property of the text from START to END."
  (when name
    (let ((pos start)
	  next prop)
      (while (< pos end)
	(setq prop (get-text-property pos 'face object)
	      next (next-single-property-change pos 'face object end))
	(unless (member name (get-text-property pos 'face object))
	  (add-text-properties pos next (list 'face (cons name prop)) object))
	(setq pos next)))))

(defun rcirc-facify (string face)
  "Return a copy of STRING with FACE property added."
  (let ((string (or string "")))
    (rcirc-add-face 0 (length string) face string)
    string))

(defvar rcirc-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (if (string-match "[[:digit:]]" "1") ;; Support POSIX?
       (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	     (punct "!?:;.,"))
	 (concat
	  "\\(?:"
	  ;; Match paired parentheses, e.g. in Wikipedia URLs:
	  "[" chars punct "]+" "(" "[" chars punct "]+" "[" chars "]*)" "[" chars "]"
	  "\\|"
	  "[" chars punct     "]+" "[" chars "]"
	  "\\)"))
     (concat ;; XEmacs 21.4 doesn't support POSIX.
      "\\([-a-z0-9_=!?#$@~%&*+\\/:;.,]\\|\\w\\)+"
      "\\([-a-z0-9_=#$@~%&*+\\/]\\|\\w\\)"))
   "\\)")
  "Regexp matching URLs.  Set to nil to disable URL features in rcirc.")

(defun rcirc-browse-url (&optional arg)
  "Prompt for URL to browse based on URLs in buffer."
  (interactive "P")
  (let ((completions (mapcar (lambda (x) (cons x nil)) rcirc-urls))
        (initial-input (car rcirc-urls))
        (history (cdr rcirc-urls)))
    (browse-url (completing-read "rcirc browse-url: "
                                 completions nil nil initial-input 'history)
                arg)))

(defun rcirc-markup-timestamp (sender response)
  (goto-char (point-min))
  (insert (rcirc-facify (format-time-string rcirc-time-format)
			'rcirc-timestamp)))

(defun rcirc-markup-attributes (sender response)
  (while (re-search-forward "\\([\C-b\C-_\C-v]\\).*?\\(\\1\\|\C-o\\)" nil t)
    (rcirc-add-face (match-beginning 0) (match-end 0)
		    (case (char-after (match-beginning 1))
		      (?\C-b 'bold)
		      (?\C-v 'italic)
		      (?\C-_ 'underline)))
    ;; keep the ^O since it could terminate other attributes
    (when (not (eq ?\C-o (char-before (match-end 2))))
      (delete-region (match-beginning 2) (match-end 2)))
    (delete-region (match-beginning 1) (match-end 1))
    (goto-char (match-beginning 1)))
  ;; remove the ^O characters now
  (goto-char (point-min))
  (while (re-search-forward "\C-o+" nil t)
    (delete-region (match-beginning 0) (match-end 0))))

(defun rcirc-markup-my-nick (sender response)
  (with-syntax-table rcirc-nick-syntax-table
    (while (re-search-forward (concat "\\b"
				      (regexp-quote (rcirc-nick
						     (rcirc-buffer-process)))
				      "\\b")
			      nil t)
      (rcirc-add-face (match-beginning 0) (match-end 0)
		      'rcirc-nick-in-message)
      (when (string= response "PRIVMSG")
	(rcirc-add-face (point-min) (point-max)
			'rcirc-nick-in-message-full-line)
	(rcirc-record-activity (current-buffer) 'nick)))))

(defun rcirc-markup-urls (sender response)
  (while (and rcirc-url-regexp ;; nil means disable URL catching
              (re-search-forward rcirc-url-regexp nil t))
    (let ((start (match-beginning 0))
	  (end (match-end 0))
	  (url (match-string-no-properties 0)))
      (make-button start end
		   'face 'rcirc-url
		   'follow-link t
		   'rcirc-url url
		   'action (lambda (button)
			     (browse-url (button-get button 'rcirc-url))))
      ;; record the url
      (push url rcirc-urls))))

(defun rcirc-markup-keywords (sender response)
  (when (and (string= response "PRIVMSG")
	     (not (string= sender (rcirc-nick (rcirc-buffer-process)))))
    (let* ((target (or rcirc-target ""))
	   (keywords (delq nil (mapcar (lambda (keyword)
					 (when (not (string-match keyword
								  target))
					   keyword))
				       rcirc-keywords))))
      (when keywords
	(while (re-search-forward (regexp-opt keywords 'words) nil t)
	  (rcirc-add-face (match-beginning 0) (match-end 0) 'rcirc-keyword)
	  (rcirc-record-activity (current-buffer) 'keyword))))))

(defun rcirc-markup-bright-nicks (sender response)
  (when (and rcirc-bright-nicks
	     (string= response "NAMES"))
    (with-syntax-table rcirc-nick-syntax-table
      (while (re-search-forward (regexp-opt rcirc-bright-nicks 'words) nil t)
	(rcirc-add-face (match-beginning 0) (match-end 0)
			'rcirc-bright-nick)))))

(defun rcirc-markup-fill (sender response)
  (when (not (string= response "372")) 	; /motd
    (let ((fill-prefix
	   (or rcirc-fill-prefix
	       (make-string (- (point) (line-beginning-position)) ?\s)))
	  (fill-column (- (cond ((eq rcirc-fill-column 'frame-width)
				 (1- (frame-width)))
				(rcirc-fill-column
				 rcirc-fill-column)
				(t fill-column))
			  ;; make sure ... doesn't cause line wrapping
			  3)))
      (fill-region (point) (point-max) nil t))))

;;; handlers
;; these are called with the server PROCESS, the SENDER, which is a
;; server or a user, depending on the command, the ARGS, which is a
;; list of strings, and the TEXT, which is the original server text,
;; verbatim
(defun rcirc-handler-001 (process sender args text)
  (rcirc-handler-generic process "001" sender args text)
  (with-rcirc-process-buffer process
    (setq rcirc-connecting nil)
    (rcirc-reschedule-timeout process)
    (setq rcirc-server-name sender)
    (setq rcirc-nick (car args))
    (rcirc-update-prompt)
    (if rcirc-auto-authenticate-flag
        (if (and rcirc-authenticate-before-join
		 ;; We have to ensure that there's an authentication
		 ;; entry for that server.  Else,
		 ;; rcirc-authenticated-hook won't be triggered, and
		 ;; autojoin won't happen at all.
		 (let (auth-required)
		   (dolist (s rcirc-authinfo auth-required)
		     (when (string-match (car s) rcirc-server-name)
		       (setq auth-required t)))))
            (progn
	      (add-hook 'rcirc-authenticated-hook 'rcirc-join-channels-post-auth t t)
              (rcirc-authenticate))
          (rcirc-authenticate)
          (rcirc-join-channels process rcirc-startup-channels))
      (rcirc-join-channels process rcirc-startup-channels))))

(defun rcirc-join-channels-post-auth (process)
  "Join `rcirc-startup-channels' after authenticating."
  (with-rcirc-process-buffer process
    (rcirc-join-channels process rcirc-startup-channels)))

(defun rcirc-handler-PRIVMSG (process sender args text)
  (rcirc-check-auth-status process sender args text)
  (let ((target (if (rcirc-channel-p (car args))
                    (car args)
                  sender))
        (message (or (cadr args) "")))
    (if (string-match "^\C-a\\(.*\\)\C-a$" message)
        (rcirc-handler-CTCP process target sender (match-string 1 message))
      (rcirc-print process sender "PRIVMSG" target message t))
    ;; update nick linestamp
    (with-current-buffer (rcirc-get-buffer process target t)
      (rcirc-put-nick-channel process sender target rcirc-current-line))))

(defun rcirc-handler-NOTICE (process sender args text)
  (rcirc-check-auth-status process sender args text)
  (let ((target (car args))
        (message (cadr args)))
    (if (string-match "^\C-a\\(.*\\)\C-a$" message)
        (rcirc-handler-CTCP-response process target sender
				     (match-string 1 message))
      (rcirc-print process sender "NOTICE"
		   (cond ((rcirc-channel-p target)
			  target)
			 ;;; -ChanServ- [#gnu] Welcome...
			 ((string-match "\\[\\(#[^\] ]+\\)\\]" message)
			  (match-string 1 message))
			 (sender
			  (if (string= sender (rcirc-server-name process))
			      nil	; server notice
			    sender)))
                 message t))))

(defun rcirc-check-auth-status (process sender args text)
  "Check if the user just authenticated.
If authenticated, runs `rcirc-authenticated-hook' with PROCESS as
the only argument."
  (with-rcirc-process-buffer process
    (when (and (not rcirc-user-authenticated)
               rcirc-authenticate-before-join
               rcirc-auto-authenticate-flag)
      (let ((target (car args))
            (message (cadr args)))
        (when (or
               (and ;; nickserv
                (string= sender "NickServ")
                (string= target rcirc-nick)
                (member message
                        (list
                         (format "You are now identified for \C-b%s\C-b." rcirc-nick)
			 (format "You are successfully identified as \C-b%s\C-b." rcirc-nick)
                         "Password accepted - you are now recognized."
                         )))
               (and ;; quakenet
                (string= sender "Q")
                (string= target rcirc-nick)
                (string-match "\\`You are now logged in as .+\\.\\'" message)))
          (setq rcirc-user-authenticated t)
          (run-hook-with-args 'rcirc-authenticated-hook process)
          (remove-hook 'rcirc-authenticated-hook 'rcirc-join-channels-post-auth t))))))

(defun rcirc-handler-WALLOPS (process sender args text)
  (rcirc-print process sender "WALLOPS" sender (car args) t))

(defun rcirc-handler-JOIN (process sender args text)
  (let ((channel (car args)))
    (with-current-buffer (rcirc-get-buffer-create process channel)
      ;; when recently rejoining, restore the linestamp
      (rcirc-put-nick-channel process sender channel
			      (let ((last-activity-lines
				     (rcirc-elapsed-lines process sender channel)))
				(when (and last-activity-lines
					   (< last-activity-lines rcirc-omit-threshold))
                                  (rcirc-last-line process sender channel))))
      ;; reset mode-line-process in case joining a channel with an
      ;; already open buffer (after getting kicked e.g.)
      (setq mode-line-process nil))

    (rcirc-print process sender "JOIN" channel "")

    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) sender)
      (rcirc-print process sender "JOIN" sender channel))))

;; PART and KICK are handled the same way
(defun rcirc-handler-PART-or-KICK (process response channel sender nick args)
  (rcirc-ignore-update-automatic nick)
  (if (not (string= nick (rcirc-nick process)))
      ;; this is someone else leaving
      (progn
	(rcirc-maybe-remember-nick-quit process nick channel)
	(rcirc-remove-nick-channel process nick channel))
    ;; this is us leaving
    (mapc (lambda (n)
	    (rcirc-remove-nick-channel process n channel))
	  (rcirc-channel-nicks process channel))

    ;; if the buffer is still around, make it inactive
    (let ((buffer (rcirc-get-buffer process channel)))
      (when buffer
	(rcirc-disconnect-buffer buffer)))))

(defun rcirc-handler-PART (process sender args text)
  (let* ((channel (car args))
	 (reason (cadr args))
	 (message (concat channel " " reason)))
    (rcirc-print process sender "PART" channel message)
    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) sender)
      (rcirc-print process sender "PART" sender message))

    (rcirc-handler-PART-or-KICK process "PART" channel sender sender reason)))

(defun rcirc-handler-KICK (process sender args text)
  (let* ((channel (car args))
	 (nick (cadr args))
	 (reason (caddr args))
	 (message (concat nick " " channel " " reason)))
    (rcirc-print process sender "KICK" channel message t)
    ;; print in private chat buffer if it exists
    (when (rcirc-get-buffer (rcirc-buffer-process) nick)
      (rcirc-print process sender "KICK" nick message))

    (rcirc-handler-PART-or-KICK process "KICK" channel sender nick reason)))

(defun rcirc-maybe-remember-nick-quit (process nick channel)
  "Remember NICK as leaving CHANNEL if they recently spoke."
  (let ((elapsed-lines (rcirc-elapsed-lines process nick channel)))
    (when (and elapsed-lines
	       (< elapsed-lines rcirc-omit-threshold))
      (let ((buffer (rcirc-get-buffer process channel)))
	(when buffer
	  (with-current-buffer buffer
	    (let ((record (assoc-string nick rcirc-recent-quit-alist t))
		  (line (rcirc-last-line process nick channel)))
	      (if record
		  (setcdr record line)
		(setq rcirc-recent-quit-alist
		      (cons (cons nick line)
			    rcirc-recent-quit-alist))))))))))

(defun rcirc-handler-QUIT (process sender args text)
  (rcirc-ignore-update-automatic sender)
  (mapc (lambda (channel)
	  ;; broadcast quit message each channel
	  (rcirc-print process sender "QUIT" channel (apply 'concat args))
	  ;; record nick in quit table if they recently spoke
	  (rcirc-maybe-remember-nick-quit process sender channel))
	(rcirc-nick-channels process sender))
  (rcirc-nick-remove process sender))

(defun rcirc-handler-NICK (process sender args text)
  (let* ((old-nick sender)
         (new-nick (car args))
         (channels (rcirc-nick-channels process old-nick)))
    ;; update list of ignored nicks
    (rcirc-ignore-update-automatic old-nick)
    (when (member old-nick rcirc-ignore-list)
      (add-to-list 'rcirc-ignore-list new-nick)
      (add-to-list 'rcirc-ignore-list-automatic new-nick))
    ;; print message to nick's channels
    (dolist (target channels)
      (rcirc-print process sender "NICK" target new-nick))
    ;; update private chat buffer, if it exists
    (let ((chat-buffer (rcirc-get-buffer process old-nick)))
      (when chat-buffer
	(with-current-buffer chat-buffer
	  (rcirc-print process sender "NICK" old-nick new-nick)
	  (setq rcirc-target new-nick)
	  (rename-buffer (rcirc-generate-new-buffer-name process new-nick)))))
    ;; remove old nick and add new one
    (with-rcirc-process-buffer process
      (let ((v (gethash old-nick rcirc-nick-table)))
        (remhash old-nick rcirc-nick-table)
        (puthash new-nick v rcirc-nick-table))
      ;; if this is our nick...
      (when (string= old-nick rcirc-nick)
        (setq rcirc-nick new-nick)
	(rcirc-update-prompt t)
        ;; reauthenticate
        (when rcirc-auto-authenticate-flag (rcirc-authenticate))))))

(defun rcirc-handler-PING (process sender args text)
  (rcirc-send-string process (concat "PONG :" (car args))))

(defun rcirc-handler-PONG (process sender args text)
  ;; do nothing
  )

(defun rcirc-handler-TOPIC (process sender args text)
  (let ((topic (cadr args)))
    (rcirc-print process sender "TOPIC" (car args) topic)
    (with-current-buffer (rcirc-get-buffer process (car args))
      (setq rcirc-topic topic))))

(defvar rcirc-nick-away-alist nil)
(defun rcirc-handler-301 (process sender args text)
  "RPL_AWAY"
  (let* ((nick (cadr args))
	 (rec (assoc-string nick rcirc-nick-away-alist))
	 (away-message (caddr args)))
    (when (or (not rec)
	      (not (string= (cdr rec) away-message)))
      ;; away message has changed
      (rcirc-handler-generic process "AWAY" nick (cdr args) text)
      (if rec
	  (setcdr rec away-message)
	(setq rcirc-nick-away-alist (cons (cons nick away-message)
					  rcirc-nick-away-alist))))))

(defun rcirc-handler-317 (process sender args text)
  "RPL_WHOISIDLE"
  (let* ((nick (nth 1 args))
         (idle-secs (string-to-number (nth 2 args)))
         (idle-string
          (if (< idle-secs most-positive-fixnum)
              (format-seconds "%yy %dd %hh %mm %z%ss" idle-secs)
            "a very long time"))
         (signon-time (seconds-to-time (string-to-number (nth 3 args))))
         (signon-string (format-time-string "%c" signon-time))
         (message (format "%s idle for %s, signed on %s"
                          nick idle-string signon-string)))
    (rcirc-print process sender "317" nil message t)))

(defun rcirc-handler-332 (process sender args text)
  "RPL_TOPIC"
  (let ((buffer (or (rcirc-get-buffer process (cadr args))
		    (rcirc-get-temp-buffer-create process (cadr args)))))
    (with-current-buffer buffer
      (setq rcirc-topic (caddr args)))))

(defun rcirc-handler-333 (process sender args text)
  "333 says who set the topic and when.
Not in rfc1459.txt"
  (let ((buffer (or (rcirc-get-buffer process (cadr args))
		    (rcirc-get-temp-buffer-create process (cadr args)))))
    (with-current-buffer buffer
      (let ((setter (caddr args))
	    (time (current-time-string
		   (seconds-to-time
		    (string-to-number (cadddr args))))))
	(rcirc-print process sender "TOPIC" (cadr args)
		     (format "%s (%s on %s)" rcirc-topic setter time))))))

(defun rcirc-handler-477 (process sender args text)
  "ERR_NOCHANMODES"
  (rcirc-print process sender "477" (cadr args) (caddr args)))

(defun rcirc-handler-MODE (process sender args text)
  (let ((target (car args))
        (msg (mapconcat 'identity (cdr args) " ")))
    (rcirc-print process sender "MODE"
                 (if (string= target (rcirc-nick process))
                     nil
                   target)
                 msg)

    ;; print in private chat buffers if they exist
    (mapc (lambda (nick)
	    (when (rcirc-get-buffer process nick)
	      (rcirc-print process sender "MODE" nick msg)))
	  (cddr args))))

(defun rcirc-get-temp-buffer-create (process channel)
  "Return a buffer based on PROCESS and CHANNEL."
  (let ((tmpnam (concat " " (downcase channel) "TMP" (process-name process))))
    (get-buffer-create tmpnam)))

(defun rcirc-handler-353 (process sender args text)
  "RPL_NAMREPLY"
  (let ((channel (nth 2 args))
	(names (or (nth 3 args) "")))
    (mapc (lambda (nick)
            (rcirc-put-nick-channel process nick channel))
          (split-string names " " t))
    ;; create a temporary buffer to insert the names into
    ;; rcirc-handler-366 (RPL_ENDOFNAMES) will handle it
    (with-current-buffer (rcirc-get-temp-buffer-create process channel)
      (goto-char (point-max))
      (insert (car (last args)) " "))))

(defun rcirc-handler-366 (process sender args text)
  "RPL_ENDOFNAMES"
  (let* ((channel (cadr args))
         (buffer (rcirc-get-temp-buffer-create process channel)))
    (with-current-buffer buffer
      (rcirc-print process sender "NAMES" channel
                   (let ((content (buffer-substring (point-min) (point-max))))
		     (rcirc-sort-nicknames-join content " "))))
    (kill-buffer buffer)))

(defun rcirc-handler-433 (process sender args text)
  "ERR_NICKNAMEINUSE"
  (rcirc-handler-generic process "433" sender args text)
  (let* ((new-nick (concat (cadr args) "`")))
    (with-rcirc-process-buffer process
      (rcirc-cmd-nick new-nick nil process))))

(defun rcirc-authenticate ()
  "Send authentication to process associated with current buffer.
Passwords are stored in `rcirc-authinfo' (which see)."
  (interactive)
  (with-rcirc-server-buffer
    (dolist (i rcirc-authinfo)
      (let ((process (rcirc-buffer-process))
	    (server (car i))
	    (nick (caddr i))
	    (method (cadr i))
	    (args (cdddr i)))
	(when (and (string-match server rcirc-server))
          (if (and (memq method '(nickserv chanserv bitlbee))
                   (string-match nick rcirc-nick))
              ;; the following methods rely on the user's nickname.
              (case method
                (nickserv
                 (rcirc-send-privmsg
                  process
                  (or (cadr args) "NickServ")
                  (concat "IDENTIFY " (car args))))
                (chanserv
                 (rcirc-send-privmsg
                  process
                  "ChanServ"
                  (format "IDENTIFY %s %s" (car args) (cadr args))))
                (bitlbee
                 (rcirc-send-privmsg
                  process
                  "&bitlbee"
                  (concat "IDENTIFY " (car args)))))
            ;; quakenet authentication doesn't rely on the user's nickname.
            ;; the variable `nick' here represents the Q account name.
            (when (eq method 'quakenet)
              (rcirc-send-privmsg
               process
               "Q@CServe.quakenet.org"
               (format "AUTH %s %s" nick (car args))))))))))

(defun rcirc-handler-INVITE (process sender args text)
  (rcirc-print process sender "INVITE" nil (mapconcat 'identity args " ") t))

(defun rcirc-handler-ERROR (process sender args text)
  (rcirc-print process sender "ERROR" nil (mapconcat 'identity args " ")))

(defun rcirc-handler-CTCP (process target sender text)
  (if (string-match "^\\([^ ]+\\) *\\(.*\\)$" text)
      (let* ((request (upcase (match-string 1 text)))
             (args (match-string 2 text))
             (handler (intern-soft (concat "rcirc-handler-ctcp-" request))))
        (if (not (fboundp handler))
            (rcirc-print process sender "ERROR" target
                         (format "%s sent unsupported ctcp: %s" sender text)
			 t)
          (funcall handler process target sender args)
          (unless (or (string= request "ACTION")
		      (string= request "KEEPALIVE"))
              (rcirc-print process sender "CTCP" target
			   (format "%s" text) t))))))

(defun rcirc-handler-ctcp-VERSION (process target sender args)
  (rcirc-send-string process
                     (concat "NOTICE " sender
                             " :\C-aVERSION " rcirc-id-string
                             "\C-a")))

(defun rcirc-handler-ctcp-ACTION (process target sender args)
  (rcirc-print process sender "ACTION" target args t))

(defun rcirc-handler-ctcp-TIME (process target sender args)
  (rcirc-send-string process
                     (concat "NOTICE " sender
                             " :\C-aTIME " (current-time-string) "\C-a")))

(defun rcirc-handler-CTCP-response (process target sender message)
  (rcirc-print process sender "CTCP" nil message t))

(defgroup rcirc-faces nil
  "Faces for rcirc."
  :group 'rcirc
  :group 'faces)

(defface rcirc-my-nick			; font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "The face used to highlight my messages."
  :group 'rcirc-faces)

(defface rcirc-other-nick	     ; font-lock-variable-name-face
  '((((class grayscale) (background light))
     (:foreground "Gray90" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 8)) (:foreground "yellow" :weight light))
    (t (:weight bold :slant italic)))
  "The face used to highlight other messages."
  :group 'rcirc-faces)

(defface rcirc-bright-nick
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Face used for nicks matched by `rcirc-bright-nicks'."
  :group 'rcirc-faces)

(defface rcirc-dim-nick
  '((t :inherit default))
  "Face used for nicks in `rcirc-dim-nicks'."
  :group 'rcirc-faces)

(defface rcirc-server			; font-lock-comment-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     )
    (((class color) (min-colors 8) (background dark))
     )
    (t (:weight bold :slant italic)))
  "The face used to highlight server messages."
  :group 'rcirc-faces)

(defface rcirc-server-prefix	 ; font-lock-comment-delimiter-face
  '((default :inherit rcirc-server)
    (((class grayscale)))
    (((class color) (min-colors 16)))
    (((class color) (min-colors 8) (background light))
     :foreground "red")
    (((class color) (min-colors 8) (background dark))
     :foreground "red1"))
  "The face used to highlight server prefixes."
  :group 'rcirc-faces)

(defface rcirc-timestamp
  '((t (:inherit default)))
  "The face used to highlight timestamps."
  :group 'rcirc-faces)

(defface rcirc-nick-in-message		; font-lock-keyword-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "The face used to highlight instances of your nick within messages."
  :group 'rcirc-faces)

(defface rcirc-nick-in-message-full-line
  '((t (:bold t)))
  "The face used emphasize the entire message when your nick is mentioned."
  :group 'rcirc-faces)

(defface rcirc-prompt			; comint-highlight-prompt
  '((((min-colors 88) (background dark)) (:foreground "cyan1"))
    (((background dark)) (:foreground "cyan"))
    (t (:foreground "dark blue")))
  "The face used to highlight prompts."
  :group 'rcirc-faces)

(defface rcirc-track-nick
  '((((type tty)) (:inherit default))
    (t (:inverse-video t)))
  "The face used in the mode-line when your nick is mentioned."
  :group 'rcirc-faces)

(defface rcirc-track-keyword
  '((t (:bold t )))
  "The face used in the mode-line when keywords are mentioned."
  :group 'rcirc-faces)

(defface rcirc-url
  '((t (:bold t)))
  "The face used to highlight urls."
  :group 'rcirc-faces)

(defface rcirc-keyword
  '((t (:inherit highlight)))
  "The face used to highlight keywords."
  :group 'rcirc-faces)


;; When using M-x flyspell-mode, only check words after the prompt
(put 'rcirc-mode 'flyspell-mode-predicate 'rcirc-looking-at-input)
(defun rcirc-looking-at-input ()
  "Returns true if point is past the input marker."
  (>= (point) rcirc-prompt-end-marker))


(provide 'rcirc)

;;; rcirc.el ends here
