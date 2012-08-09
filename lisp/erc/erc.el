;; erc.el --- An Emacs Internet Relay Chat client

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Alexander L. Belikoff (alexander@belikoff.net)
;; Contributors: Sergey Berezin (sergey.berezin@cs.cmu.edu),
;;               Mario Lang (mlang@delysid.org),
;;               Alex Schroeder (alex@gnu.org)
;;               Andreas Fuchs (afs@void.at)
;;               Gergely Nagy (algernon@midgard.debian.net)
;;               David Edmondson (dme@dme.org)
;; Maintainer: Michael Olson (mwolson@gnu.org)
;; Keywords: IRC, chat, client, Internet
;; Version: 5.3

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

;; ERC is a powerful, modular, and extensible IRC client for Emacs.

;; For more information, see the following URLs:
;; * http://sv.gnu.org/projects/erc/
;; * http://www.emacswiki.org/cgi-bin/wiki/ERC

;; As of 2006-06-13, ERC development is now hosted on Savannah
;; (http://sv.gnu.org/projects/erc).  I invite everyone who wants to
;; hack on it to contact me <mwolson@gnu.org> in order to get write
;; access to the shared Arch archive.

;; Installation:

;; Put erc.el in your load-path, and put (require 'erc) in your .emacs.

;; Configuration:

;; Use M-x customize-group RET erc RET to get an overview
;; of all the variables you can tweak.

;; Usage:

;; To connect to an IRC server, do
;;
;; M-x erc RET
;;
;; After you are connected to a server, you can use C-h m or have a look at
;; the ERC menu.

;;; History:
;;

;;; Code:

(defconst erc-version-string "Version 5.3"
  "ERC version.  This is used by function `erc-version'.")

(eval-when-compile (require 'cl))
(require 'font-lock)
(require 'pp)
(require 'thingatpt)
(require 'erc-compat)

(defvar erc-official-location
  "http://emacswiki.org/cgi-bin/wiki/ERC (mailing list: erc-discuss@gnu.org)"
  "Location of the ERC client on the Internet.")

(defgroup erc nil
  "Emacs Internet Relay Chat client."
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki/ERC")
  :prefix "erc-"
  :group 'applications)

(defgroup erc-buffers nil
  "Creating new ERC buffers"
  :group 'erc)

(defgroup erc-display nil
  "Settings for how various things are displayed"
  :group 'erc)

(defgroup erc-mode-line-and-header nil
  "Displaying information in the mode-line and header"
  :group 'erc-display)

(defgroup erc-ignore nil
  "Ignoring certain messages"
  :group 'erc)

(defgroup erc-query nil
  "Using separate buffers for private discussions"
  :group 'erc)

(defgroup erc-quit-and-part nil
  "Quitting and parting channels"
  :group 'erc)

(defgroup erc-paranoia nil
  "Know what is sent and received; control the display of sensitive data."
  :group 'erc)

(defgroup erc-scripts nil
  "Running scripts at startup and with /LOAD"
  :group 'erc)

(require 'erc-backend)

;; compatibility with older ERC releases

(if (fboundp 'defvaralias)
    (progn
      (defvaralias 'erc-announced-server-name 'erc-server-announced-name)
      (erc-make-obsolete-variable 'erc-announced-server-name
				  'erc-server-announced-name
				  "ERC 5.1")
      (defvaralias 'erc-process 'erc-server-process)
      (erc-make-obsolete-variable 'erc-process 'erc-server-process "ERC 5.1")
      (defvaralias 'erc-default-coding-system 'erc-server-coding-system)
      (erc-make-obsolete-variable 'erc-default-coding-system
				  'erc-server-coding-system
				  "ERC 5.1"))
  (message (concat "ERC: The function `defvaralias' is not bound.  See the "
		   "NEWS file for variable name changes since ERC 5.0.4.")))

(defalias 'erc-send-command 'erc-server-send)
(erc-make-obsolete 'erc-send-command 'erc-server-send "ERC 5.1")

;; tunable connection and authentication parameters

(defcustom erc-server nil
  "IRC server to use if one is not provided.
See function `erc-compute-server' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const :tag "None" nil)
		 (string :tag "Server")))

(defcustom erc-port nil
  "IRC port to use if not specified.

This can be either a string or a number."
  :group 'erc
  :type '(choice (const :tag "None" nil)
		 (integer :tag "Port number")
		 (string :tag "Port string")))

(defcustom erc-nick nil
  "Nickname to use if one is not provided.

This can be either a string, or a list of strings.
In the latter case, if the first nick in the list is already in use,
other nicks are tried in the list order.

See function `erc-compute-nick' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const :tag "None" nil)
		 (string :tag "Nickname")
		 (repeat (string :tag "Nickname"))))

(defcustom erc-nick-uniquifier "`"
  "The string to append to the nick if it is already in use."
  :group 'erc
  :type 'string)

(defcustom erc-try-new-nick-p t
  "If the nickname you chose isn't available, and this option is non-nil,
ERC should automatically attempt to connect with another nickname.

You can manually set another nickname with the /NICK command."
  :group 'erc
  :type 'boolean)

(defcustom erc-user-full-name nil
  "User full name.

This can be either a string or a function to call.

See function `erc-compute-full-name' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const :tag "No name" nil)
		 (string :tag "Name")
		 (function :tag "Get from function"))
  :set (lambda (sym val)
	 (if (functionp val)
	     (set sym (funcall val))
	   (set sym val))))

(defvar erc-password nil
  "Password to use when authenticating to an IRC server.
It is not strictly necessary to provide this, since ERC will
prompt you for it.")

(defcustom erc-user-mode nil
  "Initial user modes to be set after a connection is established."
  :group 'erc
  :type '(choice (const nil) string function))


(defcustom erc-prompt-for-password t
  "Asks before using the default password, or whether to enter a new one."
  :group 'erc
  :type 'boolean)

(defcustom erc-warn-about-blank-lines t
  "Warn the user if they attempt to send a blank line."
  :group 'erc
  :type 'boolean)

(defcustom erc-send-whitespace-lines nil
  "If set to non-nil, send lines consisting of only whitespace."
  :group 'erc
  :type 'boolean)

(defcustom erc-hide-prompt nil
  "If non-nil, do not display the prompt for commands.

\(A command is any input starting with a '/').

See also the variables `erc-prompt' and `erc-command-indicator'."
  :group 'erc-display
  :type 'boolean)

;; tunable GUI stuff

(defcustom erc-show-my-nick t
  "If non-nil, display one's own nickname when sending a message.

If non-nil, \"<nickname>\" will be shown.
If nil, only \"> \" will be shown."
  :group 'erc-display
  :type 'boolean)

(define-widget 'erc-message-type 'set
  "A set of standard IRC Message types."
  :args '((const "JOIN")
	  (const "KICK")
	  (const "NICK")
	  (const "PART")
	  (const "QUIT")
	  (const "MODE")
	  (repeat :inline t :tag "Others" (string :tag "IRC Message Type"))))

(defcustom erc-hide-list nil
  "*List of IRC type messages to hide.
A typical value would be '(\"JOIN\" \"PART\" \"QUIT\")."
  :group 'erc-ignore
  :type 'erc-message-type)

(defvar erc-session-password nil
  "The password used for the current session.")
(make-variable-buffer-local 'erc-session-password)

(defcustom erc-disconnected-hook nil
  "Run this hook with arguments (NICK IP REASON) when disconnected.
This happens before automatic reconnection.  Note, that
`erc-server-QUIT-functions' might not be run when we disconnect,
simply because we do not necessarily receive the QUIT event."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-complete-functions nil
  "These functions get called when the user hits TAB in ERC.
Each function in turn is called until one returns non-nil to
indicate it has handled the input."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-join-hook nil
  "Hook run when we join a channel.  Hook functions are called
without arguments, with the current buffer set to the buffer of
the new channel.

See also `erc-server-JOIN-functions', `erc-part-hook'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-quit-hook nil
  "Hook run when processing a quit command directed at our nick.

The hook receives one argument, the current PROCESS.
See also `erc-server-QUIT-functions' and `erc-disconnected-hook'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-part-hook nil
  "Hook run when processing a PART message directed at our nick.

The hook receives one argument, the current BUFFER.
See also `erc-server-QUIT-functions', `erc-quit-hook' and
`erc-disconnected-hook'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-kick-hook nil
  "Hook run when processing a KICK message directed at our nick.

The hook receives one argument, the current BUFFER.
See also `erc-server-PART-functions' and `erc-part-hook'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-nick-changed-functions nil
  "List of functions run when your nick was successfully changed.

Each function should accept two arguments, NEW-NICK and OLD-NICK."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-connect-pre-hook '(erc-initialize-log-marker)
  "Hook called just before `erc' calls `erc-connect'.
Functions are passed a buffer as the first argument."
  :group 'erc-hooks
  :type 'hook)


(defvar erc-channel-users nil
  "A hash table of members in the current channel, which
associates nicknames with cons cells of the form:
\(USER . MEMBER-DATA) where USER is a pointer to an
erc-server-user struct, and MEMBER-DATA is a pointer to an
erc-channel-user struct.")
(make-variable-buffer-local 'erc-channel-users)

(defvar erc-server-users nil
  "A hash table of users on the current server, which associates
nicknames with erc-server-user struct instances.")
(make-variable-buffer-local 'erc-server-users)

(defun erc-downcase (string)
  "Convert STRING to IRC standard conforming downcase."
  (let ((s (downcase string))
	(c '((?\[ . ?\{)
	     (?\] . ?\})
	     (?\\ . ?\|)
	     (?~  . ?^))))
    (save-match-data
      (while (string-match "[]\\[~]" s)
	(aset s (match-beginning 0)
	      (cdr (assq (aref s (match-beginning 0)) c)))))
    s))

(defmacro erc-with-server-buffer (&rest body)
  "Execute BODY in the current ERC server buffer.
If no server buffer exists, return nil."
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (erc-server-buffer)))
       (when (buffer-live-p ,buffer)
	 (with-current-buffer ,buffer
	   ,@body)))))
(put 'erc-with-server-buffer 'lisp-indent-function 0)
(put 'erc-with-server-buffer 'edebug-form-spec '(body))

(defstruct (erc-server-user (:type vector) :named)
  ;; User data
  nickname host login full-name info
  ;; Buffers
  ;;
  ;; This is an alist of the form (BUFFER . CHANNEL-DATA), where
  ;; CHANNEL-DATA is either nil or an erc-channel-user struct.
  (buffers nil)
  )

(defstruct (erc-channel-user (:type vector) :named)
  op voice
  ;; Last message time (in the form of the return value of
  ;; (current-time)
  ;;
  ;; This is useful for ordered name completion.
  (last-message-time nil))

(defsubst erc-get-channel-user (nick)
  "Finds the (USER . CHANNEL-DATA) element corresponding to NICK
in the current buffer's `erc-channel-users' hash table."
  (gethash (erc-downcase nick) erc-channel-users))

(defsubst erc-get-server-user (nick)
  "Finds the USER corresponding to NICK in the current server's
`erc-server-users' hash table."
  (erc-with-server-buffer
    (gethash (erc-downcase nick) erc-server-users)))

(defsubst erc-add-server-user (nick user)
  "This function is for internal use only.

Adds USER with nickname NICK to the `erc-server-users' hash table."
  (erc-with-server-buffer
    (puthash (erc-downcase nick) user erc-server-users)))

(defsubst erc-remove-server-user (nick)
  "This function is for internal use only.

Removes the user with nickname NICK from the `erc-server-users'
hash table.  This user is not removed from the
`erc-channel-users' lists of other buffers.

See also: `erc-remove-user'."
  (erc-with-server-buffer
    (remhash (erc-downcase nick) erc-server-users)))

(defun erc-change-user-nickname (user new-nick)
  "This function is for internal use only.

Changes the nickname of USER to NEW-NICK in the
`erc-server-users' hash table.  The `erc-channel-users' lists of
other buffers are also changed."
  (let ((nick (erc-server-user-nickname user)))
    (setf (erc-server-user-nickname user) new-nick)
    (erc-with-server-buffer
      (remhash (erc-downcase nick) erc-server-users)
      (puthash (erc-downcase new-nick) user erc-server-users))
    (dolist (buf (erc-server-user-buffers user))
      (if (buffer-live-p buf)
	  (with-current-buffer buf
	    (let ((cdata (erc-get-channel-user nick)))
	      (remhash (erc-downcase nick) erc-channel-users)
	      (puthash (erc-downcase new-nick) cdata
		       erc-channel-users)))))))

(defun erc-remove-channel-user (nick)
  "This function is for internal use only.

Removes the user with nickname NICK from the `erc-channel-users'
list for this channel.  If this user is not in the
`erc-channel-users' list of any other buffers, the user is also
removed from the server's `erc-server-users' list.

See also: `erc-remove-server-user' and `erc-remove-user'."
  (let ((channel-data (erc-get-channel-user nick)))
    (when channel-data
      (let ((user (car channel-data)))
	(setf (erc-server-user-buffers user)
	      (delq (current-buffer)
		    (erc-server-user-buffers user)))
	(remhash (erc-downcase nick) erc-channel-users)
	(if (null (erc-server-user-buffers user))
	    (erc-remove-server-user nick))))))

(defun erc-remove-user (nick)
  "This function is for internal use only.

Removes the user with nickname NICK from the `erc-server-users'
list as well as from all `erc-channel-users' lists.

See also: `erc-remove-server-user' and
`erc-remove-channel-user'."
  (let ((user (erc-get-server-user nick)))
    (when user
      (let ((buffers (erc-server-user-buffers user)))
	(dolist (buf buffers)
	  (if (buffer-live-p buf)
	      (with-current-buffer buf
		(remhash (erc-downcase nick) erc-channel-users)
		(run-hooks 'erc-channel-members-changed-hook)))))
      (erc-remove-server-user nick))))

(defun erc-remove-channel-users ()
  "This function is for internal use only.

Removes all users in the current channel.  This is called by
`erc-server-PART' and `erc-server-QUIT'."
  (when (and erc-server-connected
	     (erc-server-process-alive)
	     (hash-table-p erc-channel-users))
    (maphash (lambda (nick cdata)
	       (erc-remove-channel-user nick))
	     erc-channel-users)
    (clrhash erc-channel-users)))

(defsubst erc-channel-user-op-p (nick)
  "Return t if NICK is an operator in the current channel."
  (and nick
       (hash-table-p erc-channel-users)
       (let ((cdata (erc-get-channel-user nick)))
	 (and cdata (cdr cdata)
	      (erc-channel-user-op (cdr cdata))))))

(defsubst erc-channel-user-voice-p (nick)
  "Return t if NICK has voice in the current channel."
  (and nick
       (hash-table-p erc-channel-users)
       (let ((cdata (erc-get-channel-user nick)))
	 (and cdata (cdr cdata)
	      (erc-channel-user-voice (cdr cdata))))))

(defun erc-get-channel-user-list ()
  "Returns a list of users in the current channel.  Each element
of the list is of the form (USER . CHANNEL-DATA), where USER is
an erc-server-user struct, and CHANNEL-DATA is either `nil' or an
erc-channel-user struct.

See also: `erc-sort-channel-users-by-activity'"
  (let (users)
    (if (hash-table-p erc-channel-users)
      (maphash (lambda (nick cdata)
		 (setq users (cons cdata users)))
	       erc-channel-users))
    users))

(defun erc-get-server-nickname-list ()
  "Returns a list of known nicknames on the current server."
  (erc-with-server-buffer
    (let (nicks)
      (when (hash-table-p erc-server-users)
	(maphash (lambda (n user)
		   (setq nicks
			 (cons (erc-server-user-nickname user)
			       nicks)))
		 erc-server-users)
	nicks))))

(defun erc-get-channel-nickname-list ()
  "Returns a list of known nicknames on the current channel."
  (let (nicks)
    (when (hash-table-p erc-channel-users)
      (maphash (lambda (n cdata)
		 (setq nicks
		       (cons (erc-server-user-nickname (car cdata))
			     nicks)))
	       erc-channel-users)
      nicks)))

(defun erc-get-server-nickname-alist ()
  "Returns an alist of known nicknames on the current server."
  (erc-with-server-buffer
    (let (nicks)
      (when (hash-table-p erc-server-users)
	(maphash (lambda (n user)
		   (setq nicks
			 (cons (cons (erc-server-user-nickname user) nil)
			       nicks)))
		 erc-server-users)
	nicks))))

(defun erc-get-channel-nickname-alist ()
  "Returns an alist of known nicknames on the current channel."
  (let (nicks)
    (when (hash-table-p erc-channel-users)
      (maphash (lambda (n cdata)
		 (setq nicks
		       (cons (cons (erc-server-user-nickname (car cdata)) nil)
			     nicks)))
	       erc-channel-users)
      nicks)))

(defun erc-sort-channel-users-by-activity (list)
  "Sorts LIST such that users which have spoken most recently are
listed first.  LIST must be of the form (USER . CHANNEL-DATA).

See also: `erc-get-channel-user-list'."
  (sort list
	(lambda (x y)
	  (when (and
		 (cdr x) (cdr y))
	    (let ((tx (erc-channel-user-last-message-time (cdr x)))
		  (ty (erc-channel-user-last-message-time (cdr y))))
	      (if tx
		  (if ty
		      (time-less-p ty tx)
		    t)
		nil))))))

(defun erc-sort-channel-users-alphabetically (list)
  "Sort LIST so that users' nicknames are in alphabetical order.
LIST must be of the form (USER . CHANNEL-DATA).

See also: `erc-get-channel-user-list'."
  (sort list
	(lambda (x y)
	  (when (and
		 (cdr x) (cdr y))
	    (let ((nickx (downcase (erc-server-user-nickname (car x))))
		  (nicky (downcase (erc-server-user-nickname (car y)))))
	      (if nickx
		  (if nicky
		      (string-lessp nickx nicky)
		    t)
		nil))))))

(defvar erc-channel-topic nil
  "A topic string for the channel.  Should only be used in channel-buffers.")
(make-variable-buffer-local 'erc-channel-topic)

(defvar erc-channel-modes nil
  "List of strings representing channel modes.
E.g. '(\"i\" \"m\" \"s\" \"b Quake!*@*\")
\(not sure the ban list will be here, but why not)")
(make-variable-buffer-local 'erc-channel-modes)

(defvar erc-insert-marker nil
  "The place where insertion of new text in erc buffers should happen.")
(make-variable-buffer-local 'erc-insert-marker)

(defvar erc-input-marker nil
  "The marker where input should be inserted.")
(make-variable-buffer-local 'erc-input-marker)

(defun erc-string-no-properties (string)
  "Return a copy of STRING will all text-properties removed."
  (let ((newstring (copy-sequence string)))
    (set-text-properties 0 (length newstring) nil newstring)
    newstring))

(defcustom erc-prompt "ERC>"
  "Prompt used by ERC.  Trailing whitespace is not required."
  :group 'erc-display
  :type '(choice string function))

(defun erc-prompt ()
  "Return the input prompt as a string.

See also the variable `erc-prompt'."
  (let ((prompt (if (functionp erc-prompt)
		    (funcall erc-prompt)
		  erc-prompt)))
    (if (> (length prompt) 0)
	(concat prompt " ")
      prompt)))

(defcustom erc-command-indicator nil
  "Indicator used by ERC for showing commands.

If non-nil, this will be used in the ERC buffer to indicate
commands (i.e., input starting with a '/').

If nil, the prompt will be constructed from the variable `erc-prompt'."
  :group 'erc-display
  :type '(choice (const nil) string function))

(defun erc-command-indicator ()
  "Return the command indicator prompt as a string.

This only has any meaning if the variable `erc-command-indicator' is non-nil."
  (and erc-command-indicator
       (let ((prompt (if (functionp erc-command-indicator)
			 (funcall erc-command-indicator)
			 erc-command-indicator)))
	 (if (> (length prompt) 0)
	     (concat prompt " ")
	     prompt))))

(defcustom erc-notice-prefix "*** "
  "*Prefix for all notices."
  :group 'erc-display
  :type 'string)

(defcustom erc-notice-highlight-type 'all
  "*Determines how to highlight notices.
See `erc-notice-prefix'.

The following values are allowed:

    'prefix - highlight notice prefix only
    'all    - highlight the entire notice

Any other value disables notice's highlighting altogether."
  :group 'erc-display
  :type '(choice (const :tag "highlight notice prefix only" prefix)
		 (const :tag "highlight the entire notice" all)
		 (const :tag "don't highlight notices at all" nil)))

(defcustom erc-echo-notice-hook nil
  "*Specifies a list of functions to call to echo a private
notice.  Each function is called with four arguments, the string
to display, the parsed server message, the target buffer (or
nil), and the sender.  The functions are called in order, until a
function evaluates to non-nil.  These hooks are called after
those specified in `erc-echo-notice-always-hook'.

See also: `erc-echo-notice-always-hook',
`erc-echo-notice-in-default-buffer',
`erc-echo-notice-in-target-buffer',
`erc-echo-notice-in-minibuffer',
`erc-echo-notice-in-server-buffer',
`erc-echo-notice-in-active-non-server-buffer',
`erc-echo-notice-in-active-buffer',
`erc-echo-notice-in-user-buffers',
`erc-echo-notice-in-user-and-target-buffers',
`erc-echo-notice-in-first-user-buffer'"
  :group 'erc-hooks
  :type 'hook
  :options '(erc-echo-notice-in-default-buffer
	     erc-echo-notice-in-target-buffer
	     erc-echo-notice-in-minibuffer
	     erc-echo-notice-in-server-buffer
	     erc-echo-notice-in-active-non-server-buffer
	     erc-echo-notice-in-active-buffer
	     erc-echo-notice-in-user-buffers
	     erc-echo-notice-in-user-and-target-buffers
	     erc-echo-notice-in-first-user-buffer))

(defcustom erc-echo-notice-always-hook
  '(erc-echo-notice-in-default-buffer)
  "*Specifies a list of functions to call to echo a private
notice.  Each function is called with four arguments, the string
to display, the parsed server message, the target buffer (or
nil), and the sender.  The functions are called in order, and all
functions are called.  These hooks are called before those
specified in `erc-echo-notice-hook'.

See also: `erc-echo-notice-hook',
`erc-echo-notice-in-default-buffer',
`erc-echo-notice-in-target-buffer',
`erc-echo-notice-in-minibuffer',
`erc-echo-notice-in-server-buffer',
`erc-echo-notice-in-active-non-server-buffer',
`erc-echo-notice-in-active-buffer',
`erc-echo-notice-in-user-buffers',
`erc-echo-notice-in-user-and-target-buffers',
`erc-echo-notice-in-first-user-buffer'"
  :group 'erc-hooks
  :type 'hook
  :options '(erc-echo-notice-in-default-buffer
	     erc-echo-notice-in-target-buffer
	     erc-echo-notice-in-minibuffer
	     erc-echo-notice-in-server-buffer
	     erc-echo-notice-in-active-non-server-buffer
	     erc-echo-notice-in-active-buffer
	     erc-echo-notice-in-user-buffers
	     erc-echo-notice-in-user-and-target-buffers
	     erc-echo-notice-in-first-user-buffer))

;; other tunable parameters

(defcustom erc-whowas-on-nosuchnick nil
  "*If non-nil, do a whowas on a nick if no such nick."
  :group 'erc
  :type 'boolean)

(defcustom erc-verbose-server-ping nil
  "*If non-nil, show every time you get a PING or PONG from the server."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-public-away-p nil
  "*Let others know you are back when you are no longer marked away.
This happens in this form:
* <nick> is back (gone for <time>)

Many consider it impolite to do so automatically."
  :group 'erc
  :type 'boolean)

(defcustom erc-away-nickname nil
  "*The nickname to take when you are marked as being away."
  :group 'erc
  :type '(choice (const nil)
		 string))

(defcustom erc-paranoid nil
  "If non-nil, then all incoming CTCP requests will be shown."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-disable-ctcp-replies nil
  "Disable replies to CTCP requests that require a reply.
If non-nil, then all incoming CTCP requests that normally require
an automatic reply (like VERSION or PING) will be ignored.  Good to
set if some hacker is trying to flood you away."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-anonymous-login t
  "Be paranoid, don't give away your machine name."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-prompt-for-channel-key nil
  "Prompt for channel key when using `erc-join-channel' interactively."
  :group 'erc
  :type 'boolean)

(defcustom erc-email-userid "user"
  "Use this as your email user ID."
  :group 'erc
  :type 'string)

(defcustom erc-system-name nil
  "Use this as the name of your system.
If nil, ERC will call `system-name' to get this information."
  :group 'erc
  :type '(choice (const :tag "Default system name" nil)
		 string))

(defcustom erc-ignore-list nil
  "*List of regexps matching user identifiers to ignore.

A user identifier has the form \"nick!login@host\".  If an
identifier matches, the message from the person will not be
processed."
  :group 'erc-ignore
  :type '(repeat regexp))
(make-variable-buffer-local 'erc-ignore-list)

(defcustom erc-ignore-reply-list nil
  "*List of regexps matching user identifiers to ignore completely.

This differs from `erc-ignore-list' in that it also ignores any
messages directed at the user.

A user identifier has the form \"nick!login@host\".

If an identifier matches, or a message is addressed to a nick
whose identifier matches, the message will not be processed.

CAVEAT: ERC doesn't know about the user and host of anyone who
was already in the channel when you joined, but never said
anything, so it won't be able to match the user and host of those
people.  You can update the ERC internal info using /WHO *."
  :group 'erc-ignore
  :type '(repeat regexp))

(defvar erc-flood-protect t
  "*If non-nil, flood protection is enabled.
Flooding is sending too much information to the server in too
short of an interval, which may cause the server to terminate the
connection.

See `erc-server-flood-margin' for other flood-related parameters.")

;; Script parameters

(defcustom erc-startup-file-list
  (list (concat erc-user-emacs-directory ".ercrc.el")
	(concat erc-user-emacs-directory ".ercrc")
	"~/.ercrc.el" "~/.ercrc" ".ercrc.el" ".ercrc")
  "List of files to try for a startup script.
The first existent and readable one will get executed.

If the filename ends with `.el' it is presumed to be an Emacs Lisp
script and it gets (load)ed.  Otherwise it is treated as a bunch of
regular IRC commands."
  :group 'erc-scripts
  :type '(repeat file))

(defcustom erc-script-path nil
  "List of directories to look for a script in /load command.
The script is first searched in the current directory, then in each
directory in the list."
  :group 'erc-scripts
  :type '(repeat directory))

(defcustom erc-script-echo t
  "*If non-nil, echo the IRC script commands locally."
  :group 'erc-scripts
  :type 'boolean)

(defvar erc-last-saved-position nil
  "A marker containing the position the current buffer was last saved at.")
(make-variable-buffer-local 'erc-last-saved-position)

(defcustom erc-kill-buffer-on-part nil
  "Kill the channel buffer on PART.
This variable should probably stay nil, as ERC can reuse buffers if
you rejoin them later."
  :group 'erc-quit-and-part
  :type 'boolean)

(defcustom erc-kill-queries-on-quit nil
  "Kill all query (also channel) buffers of this server on QUIT.
See the variable `erc-kill-buffer-on-part' for details."
  :group 'erc-quit-and-part
  :type 'boolean)

(defcustom erc-kill-server-buffer-on-quit nil
  "Kill the server buffer of the process on QUIT."
  :group 'erc-quit-and-part
  :type 'boolean)

(defcustom erc-quit-reason-various-alist nil
  "Alist of possible arguments to the /quit command.

Each element has the form:
  (REGEXP RESULT)

If REGEXP matches the argument to /quit, then its relevant RESULT
will be used.  RESULT may be either a string, or a function.  If
a function, it should return the quit message as a string.

If no elements match, then the empty string is used.

As an example:
  (setq erc-quit-reason-various-alist
      '((\"zippy\" erc-quit-reason-zippy)
	(\"xmms\" dme:now-playing)
	(\"version\" erc-quit-reason-normal)
	(\"home\" \"Gone home !\")
	(\"^$\" \"Default Reason\")))
If the user types \"/quit zippy\", then a Zippy the Pinhead quotation
will be used as the quit message."
  :group 'erc-quit-and-part
  :type '(repeat (list regexp (choice (string) (function)))))

(defcustom erc-part-reason-various-alist nil
  "Alist of possible arguments to the /part command.

Each element has the form:
  (REGEXP RESULT)

If REGEXP matches the argument to /part, then its relevant RESULT
will be used.  RESULT may be either a string, or a function.  If
a function, it should return the part message as a string.

If no elements match, then the empty string is used.

As an example:
  (setq erc-part-reason-various-alist
      '((\"zippy\" erc-part-reason-zippy)
	(\"xmms\" dme:now-playing)
	(\"version\" erc-part-reason-normal)
	(\"home\" \"Gone home !\")
	(\"^$\" \"Default Reason\")))
If the user types \"/part zippy\", then a Zippy the Pinhead quotation
will be used as the part message."
  :group 'erc-quit-and-part
  :type '(repeat (list regexp (choice (string) (function)))))

(defcustom erc-quit-reason 'erc-quit-reason-normal
  "*A function which returns the reason for quitting.

The function is passed a single argument, the string typed by the
user after \"/quit\"."
  :group 'erc-quit-and-part
  :type '(choice (const erc-quit-reason-normal)
		 (const erc-quit-reason-zippy)
		 (const erc-quit-reason-various)
		 (symbol)))

(defcustom erc-part-reason 'erc-part-reason-normal
  "A function which returns the reason for parting a channel.

The function is passed a single argument, the string typed by the
user after \"/PART\"."
  :group 'erc-quit-and-part
  :type '(choice (const erc-part-reason-normal)
		 (const erc-part-reason-zippy)
		 (const erc-part-reason-various)
		 (symbol)))

(defvar erc-grab-buffer-name "*erc-grab*"
  "The name of the buffer created by `erc-grab-region'.")

;; variables available for IRC scripts

(defvar erc-user-information "ERC User"
  "USER_INFORMATION IRC variable.")

;; Hooks

(defgroup erc-hooks nil
  "Hook variables for fancy customizations of ERC."
  :group 'erc)

(defcustom erc-mode-hook nil
  "Hook run after `erc-mode' setup is finished."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-add-scroll-to-bottom))

(defcustom erc-timer-hook nil
  "Put functions which should get called more or less periodically here.
The idea is that servers always play ping pong with the client, and so there
is no need for any idle-timer games with Emacs."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-insert-pre-hook nil
  "Hook called first when some text is inserted through `erc-display-line'.
It gets called with one argument, STRING.
To be able to modify the inserted text, use `erc-insert-modify-hook' instead.
Filtering functions can set `erc-insert-this' to nil to avoid
display of that particular string at all."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-send-pre-hook nil
  "Hook called first when some text is sent through `erc-send-current-line'.
It gets called with one argument, STRING.

To change the text that will be sent, set the variable STR which is
used in `erc-send-current-line'.

To change the text inserted into the buffer without changing the text
that will be sent, use `erc-send-modify-hook' instead.

Filtering functions can set `erc-send-this' to nil to avoid sending of
that particular string at all and `erc-insert-this' to prevent
inserting that particular string into the buffer.

Note that it's useless to set `erc-send-this' to nil and
`erc-insert-this' to t.  ERC is sane enough to not insert the text
anyway."
  :group 'erc-hooks
  :type 'hook)

(defvar erc-insert-this t
  "Insert the text into the target buffer or not.
Functions on `erc-insert-pre-hook' can set this variable to nil
if they wish to avoid insertion of a particular string.")

(defvar erc-send-this t
  "Send the text to the target or not.
Functions on `erc-send-pre-hook' can set this variable to nil
if they wish to avoid sending of a particular string.")

(defcustom erc-insert-modify-hook ()
  "Insertion hook for functions that will change the text's appearance.
This hook is called just after `erc-insert-pre-hook' when the value
of `erc-insert-this' is t.
While this hook is run, narrowing is in effect and `current-buffer' is
the buffer where the text got inserted.  One possible value to add here
is `erc-fill'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-insert-post-hook nil
  "This hook is called just after `erc-insert-modify-hook'.
At this point, all modifications from prior hook functions are done."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-truncate-buffer
	     erc-make-read-only
	     erc-save-buffer-in-logs))

(defcustom erc-send-modify-hook nil
  "Sending hook for functions that will change the text's appearance.
This hook is called just after `erc-send-pre-hook' when the values
of `erc-send-this' and `erc-insert-this' are both t.
While this hook is run, narrowing is in effect and `current-buffer' is
the buffer where the text got inserted.

Note that no function in this hook can change the appearance of the
text that is sent.  Only changing the sent text's appearance on the
sending user's screen is possible.  One possible value to add here
is `erc-fill'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-send-post-hook nil
  "This hook is called just after `erc-send-modify-hook'.
At this point, all modifications from prior hook functions are done.
NOTE: The functions on this hook are called _before_ sending a command
to the server.

This function is called with narrowing, ala `erc-send-modify-hook'."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-make-read-only))

(defcustom erc-send-completed-hook
  (when (featurep 'emacspeak)
    (list (byte-compile
	   (lambda (str)
	     (emacspeak-auditory-icon 'select-object)))))
  "Hook called after a message has been parsed by ERC.

The single argument to the functions is the unmodified string
which the local user typed."
  :group 'erc-hooks
  :type 'hook)
;; mode-specific tables

(defvar erc-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " syntax-table)
    (modify-syntax-entry ?\\ ".   " syntax-table)
    (modify-syntax-entry ?' "w   " syntax-table)
    ;; Make dabbrev-expand useful for nick names
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    syntax-table)
  "Syntax table used while in ERC mode.")

(defvar erc-mode-abbrev-table nil
  "Abbrev table used while in ERC mode.")
(define-abbrev-table 'erc-mode-abbrev-table ())

(defvar erc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'erc-send-current-line)
    (define-key map "\C-a" 'erc-bol)
    (define-key map [home] 'erc-bol)
    (define-key map "\C-c\C-a" 'erc-bol)
    (define-key map "\C-c\C-b" 'erc-iswitchb)
    (define-key map "\C-c\C-c" 'erc-toggle-interpret-controls)
    (define-key map "\C-c\C-d" 'erc-input-action)
    (define-key map "\C-c\C-e" 'erc-toggle-ctcp-autoresponse)
    (define-key map "\C-c\C-f" 'erc-toggle-flood-control)
    (define-key map "\C-c\C-i" 'erc-invite-only-mode)
    (define-key map "\C-c\C-j" 'erc-join-channel)
    (define-key map "\C-c\C-n" 'erc-channel-names)
    (define-key map "\C-c\C-o" 'erc-get-channel-mode-from-keypress)
    (define-key map "\C-c\C-p" 'erc-part-from-channel)
    (define-key map "\C-c\C-q" 'erc-quit-server)
    (define-key map "\C-c\C-r" 'erc-remove-text-properties-region)
    (define-key map "\C-c\C-t" 'erc-set-topic)
    (define-key map "\C-c\C-u" 'erc-kill-input)
    (define-key map "\C-c\C-x" 'erc-quit-server)
    (define-key map "\M-\t" 'ispell-complete-word)
    (define-key map "\t" 'completion-at-point)

    ;; Suppress `font-lock-fontify-block' key binding since it
    ;; destroys face properties.
    (if (fboundp 'command-remapping)
	(define-key map [remap font-lock-fontify-block] 'undefined)
      (substitute-key-definition
       'font-lock-fontify-block 'undefined map global-map))

    map)
  "ERC keymap.")

;; Faces

; Honestly, I have a horrible sense of color and the "defaults" below
; are supposed to be really bad. But colors ARE required in IRC to
; convey different parts of conversation. If you think you know better
; defaults - send them to me.

;; Now colors are a bit nicer, at least to my eyes.
;; You may still want to change them to better fit your background.-- S.B.

(defgroup erc-faces nil
  "Faces for ERC."
  :group 'erc)

(defface erc-default-face '((t))
  "ERC default face."
  :group 'erc-faces)

(defface erc-direct-msg-face '((t (:foreground "IndianRed")))
  "ERC face used for messages you receive in the main erc buffer."
  :group 'erc-faces)

(defface erc-header-line
  '((t (:foreground "grey20" :background "grey90")))
  "ERC face used for the header line.

This will only be used if `erc-header-line-face-method' is non-nil."
  :group 'erc-faces)

(defface erc-input-face '((t (:foreground "brown")))
  "ERC face used for your input."
  :group 'erc-faces)

(defface erc-prompt-face
  '((t (:bold t :foreground "Black" :background "lightBlue2")))
  "ERC face for the prompt."
  :group 'erc-faces)

(defface erc-command-indicator-face
    '((t (:bold t)))
  "ERC face for the command indicator.
See the variable `erc-command-indicator'."
  :group 'erc-faces)

(defface erc-notice-face
  (if (or (featurep 'xemacs)
	  (< emacs-major-version 22))
      '((t (:bold t :foreground "blue")))
    '((((class color) (min-colors 88))
       (:bold t :foreground "SlateBlue"))
      (t (:bold t :foreground "blue"))))
  "ERC face for notices."
  :group 'erc-faces)

(defface erc-action-face '((t (:bold t)))
  "ERC face for actions generated by /ME."
  :group 'erc-faces)

(defface erc-error-face '((t (:foreground "red")))
  "ERC face for errors."
  :group 'erc-faces)

;; same default color as `erc-input-face'
(defface erc-my-nick-face '((t (:bold t :foreground "brown")))
  "ERC face for your current nickname in messages sent by you.
See also `erc-show-my-nick'."
  :group 'erc-faces)

(defface erc-nick-default-face '((t (:bold t)))
  "ERC nickname default face."
  :group 'erc-faces)

(defface erc-nick-msg-face '((t (:bold t :foreground "IndianRed")))
  "ERC nickname face for private messages."
  :group 'erc-faces)

;; Debugging support

(defvar erc-log-p nil
  "When set to t, generate debug messages in a separate debug buffer.")

(defvar erc-debug-log-file (expand-file-name "ERC.debug")
  "Debug log file name.")

(defvar erc-dbuf nil)
(make-variable-buffer-local 'erc-dbuf)

(defmacro define-erc-module (name alias doc enable-body disable-body
			     &optional local-p)
  "Define a new minor mode using ERC conventions.
Symbol NAME is the name of the module.
Symbol ALIAS is the alias to use, or nil.
DOC is the documentation string to use for the minor mode.
ENABLE-BODY is a list of expressions used to enable the mode.
DISABLE-BODY is a list of expressions used to disable the mode.
If LOCAL-P is non-nil, the mode will be created as a buffer-local
mode, rather than a global one.

This will define a minor mode called erc-NAME-mode, possibly
an alias erc-ALIAS-mode, as well as the helper functions
erc-NAME-enable, and erc-NAME-disable.

Example:

  ;;;###autoload (autoload 'erc-replace-mode \"erc-replace\")
  (define-erc-module replace nil
    \"This mode replaces incoming text according to `erc-replace-alist'.\"
    ((add-hook 'erc-insert-modify-hook
	       'erc-replace-insert))
    ((remove-hook 'erc-insert-modify-hook
		  'erc-replace-insert)))"
  (let* ((sn (symbol-name name))
	 (mode (intern (format "erc-%s-mode" (downcase sn))))
	 (group (intern (format "erc-%s" (downcase sn))))
	 (enable (intern (format "erc-%s-enable" (downcase sn))))
	 (disable (intern (format "erc-%s-disable" (downcase sn)))))
    `(progn
       (erc-define-minor-mode
	,mode
	,(format "Toggle ERC %S mode.
With a prefix argument ARG, enable %s if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.
%s" name name doc)
	nil nil nil
	:global ,(not local-p) :group (quote ,group)
	(if ,mode
	    (,enable)
	  (,disable)))
       (defun ,enable ()
	 ,(format "Enable ERC %S mode."
		  name)
	 (interactive)
	 (add-to-list 'erc-modules (quote ,name))
	 (setq ,mode t)
	 ,@enable-body)
       (defun ,disable ()
	 ,(format "Disable ERC %S mode."
		  name)
	 (interactive)
	 (setq erc-modules (delq (quote ,name) erc-modules))
	 (setq ,mode nil)
	 ,@disable-body)
       ,(when (and alias (not (eq name alias)))
	  `(defalias
	     (quote
	      ,(intern
		(format "erc-%s-mode"
			(downcase (symbol-name alias)))))
	     (quote
	      ,mode)))
       ;; For find-function and find-variable.
       (put ',mode    'definition-name ',name)
       (put ',enable  'definition-name ',name)
       (put ',disable 'definition-name ',name))))

(put 'define-erc-module 'doc-string-elt 3)

(defun erc-once-with-server-event (event &rest forms)
  "Execute FORMS the next time EVENT occurs in the `current-buffer'.

You should make sure that `current-buffer' is a server buffer.

This function temporarily adds a function to EVENT's hook to
execute FORMS.  After FORMS are run, the function is removed from
EVENT's hook.  The last expression of FORMS should be either nil
or t, where nil indicates that the other functions on EVENT's hook
should be run too, and t indicates that other functions should
not be run.

Please be sure to use this function in server-buffers.  In
channel-buffers it may not work at all, as it uses the LOCAL
argument of `add-hook' and `remove-hook' to ensure multiserver
capabilities."
  (unless (erc-server-buffer-p)
    (error
     "You should only run `erc-once-with-server-event' in a server buffer"))
  (let ((fun (make-symbol "fun"))
	(hook (erc-get-hook event)))
     (put fun 'erc-original-buffer (current-buffer))
     (fset fun `(lambda (proc parsed)
		  (with-current-buffer (get ',fun 'erc-original-buffer)
		    (remove-hook ',hook ',fun t))
		  (fmakunbound ',fun)
		  ,@forms))
     (add-hook hook fun nil t)
     fun))

(defun erc-once-with-server-event-global (event &rest forms)
  "Execute FORMS the next time EVENT occurs in any server buffer.

This function temporarily prepends a function to EVENT's hook to
execute FORMS.  After FORMS are run, the function is removed from
EVENT's hook.  The last expression of FORMS should be either nil
or t, where nil indicates that the other functions on EVENT's hook
should be run too, and t indicates that other functions should
not be run.

When FORMS execute, the current buffer is the server buffer associated with the
connection over which the data was received that triggered EVENT."
  (let ((fun (make-symbol "fun"))
	(hook (erc-get-hook event)))
     (fset fun `(lambda (proc parsed)
		  (remove-hook ',hook ',fun)
		  (fmakunbound ',fun)
		  ,@forms))
     (add-hook hook fun nil nil)
     fun))

(defmacro erc-log (string)
  "Logs STRING if logging is on (see `erc-log-p')."
  `(when erc-log-p
     (erc-log-aux ,string)))

(defun erc-server-buffer ()
  "Return the server buffer for the current buffer's process.
The buffer-local variable `erc-server-process' is used to find
the process buffer."
  (and (erc-server-buffer-live-p)
       (process-buffer erc-server-process)))

(defun erc-server-buffer-live-p ()
  "Return t if the server buffer has not been killed."
  (and (processp erc-server-process)
       (buffer-live-p (process-buffer erc-server-process))))

(defun erc-server-buffer-p (&optional buffer)
  "Return non-nil if argument BUFFER is an ERC server buffer.

If BUFFER is nil, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (and (eq major-mode 'erc-mode)
	 (null (erc-default-target)))))

(defun erc-open-server-buffer-p (&optional buffer)
  "Return non-nil if argument BUFFER is an ERC server buffer that
has an open IRC process.

If BUFFER is nil, the current buffer is used."
  (and (erc-server-buffer-p)
       (erc-server-process-alive)))

(defun erc-query-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is an ERC query buffer.
If BUFFER is nil, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (let ((target (erc-default-target)))
      (and (eq major-mode 'erc-mode)
	   target
	   (not (memq (aref target 0) '(?# ?& ?+ ?!)))))))

(defun erc-ison-p (nick)
  "Return non-nil if NICK is online."
  (interactive "sNick: ")
  (erc-with-server-buffer
    (let ((erc-online-p 'unknown))
      (erc-once-with-server-event
       303
       `(let ((ison (split-string (aref parsed 3))))
	  (setq erc-online-p (car (erc-member-ignore-case ,nick ison)))
	  t))
      (erc-server-send (format "ISON %s" nick))
      (while (eq erc-online-p 'unknown) (accept-process-output))
      (if (interactive-p)
	  (message "%s is %sonline"
		   (or erc-online-p nick)
		   (if erc-online-p "" "not "))
	erc-online-p))))

(defun erc-log-aux (string)
  "Do the debug logging of STRING."
  (let ((cb (current-buffer))
	(point 1)
	(was-eob nil)
	(session-buffer (erc-server-buffer)))
    (if session-buffer
	(progn
	  (set-buffer session-buffer)
	  (if (not (and erc-dbuf (bufferp erc-dbuf) (buffer-live-p erc-dbuf)))
	      (progn
		(setq erc-dbuf (get-buffer-create
				(concat "*ERC-DEBUG: "
					erc-session-server "*")))))
	  (set-buffer erc-dbuf)
	  (setq point (point))
	  (setq was-eob (eobp))
	  (goto-char (point-max))
	  (insert (concat "** " string "\n"))
	  (if was-eob (goto-char (point-max))
	    (goto-char point))
	  (set-buffer cb))
      (message "ERC: ** %s" string))))

;; Last active buffer, to print server messages in the right place

(defvar erc-active-buffer nil
  "The current active buffer, the one where the user typed the last command.
Defaults to the server buffer, and should only be set in the
server buffer.")
(make-variable-buffer-local 'erc-active-buffer)

(defun erc-active-buffer ()
  "Return the value of `erc-active-buffer' for the current server.
Defaults to the server buffer."
  (erc-with-server-buffer
    (if (buffer-live-p erc-active-buffer)
	erc-active-buffer
      (setq erc-active-buffer (current-buffer)))))

(defun erc-set-active-buffer (buffer)
  "Set the value of `erc-active-buffer' to BUFFER."
  (cond ((erc-server-buffer)
	 (with-current-buffer (erc-server-buffer)
	   (setq erc-active-buffer buffer)))
	(t (setq erc-active-buffer buffer))))

;; Mode activation routines

(define-derived-mode erc-mode fundamental-mode "ERC"
  "Major mode for Emacs IRC."
  (setq local-abbrev-table erc-mode-abbrev-table)
  (when (boundp 'next-line-add-newlines)
    (set (make-local-variable 'next-line-add-newlines) nil))
  (setq line-move-ignore-invisible t)
  (set (make-local-variable 'paragraph-separate)
       (concat "\C-l\\|\\(^" (regexp-quote (erc-prompt)) "\\)"))
  (set (make-local-variable 'paragraph-start)
       (concat "\\(" (regexp-quote (erc-prompt)) "\\)"))
  (add-hook 'completion-at-point-functions 'erc-complete-word-at-point nil t))

;; activation

(defconst erc-default-server "irc.freenode.net"
  "IRC server to use if it cannot be detected otherwise.")

(defconst erc-default-port 6667
  "IRC port to use if it cannot be detected otherwise.")

(defcustom erc-join-buffer 'buffer
  "Determines how to display a newly created IRC buffer.

The available choices are:

  'window          - in another window,
  'window-noselect - in another window, but don't select that one,
  'frame           - in another frame,
  'bury            - bury it in a new buffer,
  'buffer          - in place of the current buffer,
  any other value  - in place of the current buffer."
  :group 'erc-buffers
  :type '(choice (const :tag "Split window and select" window)
		 (const :tag "Split window, don't select" window-noselect)
		 (const :tag "New frame" frame)
		 (const :tag "Bury in new buffer" bury)
		 (const :tag "Use current buffer" buffer)
		 (const :tag "Use current buffer" t)))

(defcustom erc-frame-alist nil
  "*Alist of frame parameters for creating erc frames.
A value of nil means to use `default-frame-alist'."
  :group 'erc-buffers
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value"))))

(defcustom erc-frame-dedicated-flag nil
  "*Non-nil means the erc frames are dedicated to that buffer.
This only has effect when `erc-join-buffer' is set to `frame'."
  :group 'erc-buffers
  :type 'boolean)

(defcustom erc-reuse-frames t
  "*Determines whether new frames are always created.
Non-nil means that a new frame is not created to display an ERC
buffer if there is already a window displaying it.  This only has
effect when `erc-join-buffer' is set to `frame'."
  :group 'erc-buffers
  :type 'boolean)

(defun erc-channel-p (channel)
  "Return non-nil if CHANNEL seems to be an IRC channel name."
  (cond ((stringp channel)
	 (memq (aref channel 0) '(?# ?& ?+ ?!)))
	((and (bufferp channel) (buffer-live-p channel))
	 (with-current-buffer channel
	   (erc-channel-p (erc-default-target))))
	(t nil)))

(defcustom erc-reuse-buffers t
  "*If nil, create new buffers on joining a channel/query.
If non-nil, a new buffer will only be created when you join
channels with same names on different servers, or have query buffers
open with nicks of the same name on different servers.  Otherwise,
the existing buffers will be reused."
  :group 'erc-buffers
  :type 'boolean)

(defun erc-normalize-port (port)
  "Normalize the port specification PORT to integer form.
PORT may be an integer, a string or a symbol.  If it is a string or a
symbol, it may have these values:
* irc         -> 194
* ircs        -> 994
* ircd        -> 6667
* ircd-dalnet -> 7000"
  (cond
   ((symbolp port)
    (erc-normalize-port (symbol-name port)))
   ((stringp port)
    (let ((port-nr (string-to-number port)))
      (cond
       ((> port-nr 0)
	port-nr)
       ((string-equal port "irc")
	194)
       ((string-equal port "ircs")
	994)
       ((string-equal port "ircd")
	6667)
       ((string-equal port "ircd-dalnet")
	7000)
       (t
	nil))))
   ((numberp port)
    port)
   (t
    nil)))

(defun erc-port-equal (a b)
  "Check whether ports A and B are equal."
  (= (erc-normalize-port a) (erc-normalize-port b)))

(defun erc-generate-new-buffer-name (server port target &optional proc)
  "Create a new buffer name based on the arguments."
  (when (numberp port) (setq port (number-to-string port)))
  (let ((buf-name (or target
                      (or (let ((name (concat server ":" port)))
                            (when (> (length name) 1)
                              name))
                          ;; This fallback should in fact never happen
                          "*erc-server-buffer*")))
        buffer-name)
    ;; Reuse existing buffers, but not if the buffer is a connected server
    ;; buffer and not if its associated with a different server than the
    ;; current ERC buffer.
    ;; if buf-name is taken by a different connection (or by something !erc)
    ;; then see if "buf-name/server" meets the same criteria
    (dolist (candidate (list buf-name (concat buf-name "/" server)))
      (if (and (not buffer-name)
               erc-reuse-buffers
               (get-buffer candidate)
               (or target
                   (with-current-buffer (get-buffer candidate)
                     (and (erc-server-buffer-p)
                          (not (erc-server-process-alive)))))
               (with-current-buffer (get-buffer candidate)
                 (and (string= erc-session-server server)
                      (erc-port-equal erc-session-port port))))
          (setq buffer-name candidate)))
    ;; if buffer-name is unset, neither candidate worked out for us,
    ;; fallback to the old <N> uniquification method:
    (or buffer-name (generate-new-buffer-name buf-name)) ))

(defun erc-get-buffer-create (server port target &optional proc)
  "Create a new buffer based on the arguments."
  (get-buffer-create (erc-generate-new-buffer-name server port target proc)))


(defun erc-member-ignore-case (string list)
  "Return non-nil if STRING is a member of LIST.

All strings are compared according to IRC protocol case rules, see
`erc-downcase'."
  (setq string (erc-downcase string))
  (catch 'result
    (while list
      (if (string= string (erc-downcase (car list)))
	  (throw 'result list)
	(setq list (cdr list))))))

(defmacro erc-with-buffer (spec &rest body)
  "Execute BODY in the buffer associated with SPEC.

SPEC should have the form

 (TARGET [PROCESS])

If TARGET is a buffer, use it.  Otherwise, use the buffer
matching TARGET in the process specified by PROCESS.

If PROCESS is nil, use the current `erc-server-process'.
See `erc-get-buffer' for details.

See also `with-current-buffer'.

\(fn (TARGET [PROCESS]) BODY...)"
  (let ((buf (make-symbol "buf"))
	(proc (make-symbol "proc"))
	(target (make-symbol "target"))
	(process (make-symbol "process")))
    `(let* ((,target ,(car spec))
	    (,process ,(cadr spec))
	    (,buf (if (bufferp ,target)
		      ,target
		    (let ((,proc (or ,process
				     (and (processp erc-server-process)
					  erc-server-process))))
		      (if (and ,target ,proc)
			  (erc-get-buffer ,target ,proc))))))
       (when (buffer-live-p ,buf)
	 (with-current-buffer ,buf
	   ,@body)))))
(put 'erc-with-buffer 'lisp-indent-function 1)
(put 'erc-with-buffer 'edebug-form-spec '((form &optional form) body))

(defun erc-get-buffer (target &optional proc)
  "Return the buffer matching TARGET in the process PROC.
If PROC is not supplied, all processes are searched."
  (let ((downcased-target (erc-downcase target)))
    (catch 'buffer
      (erc-buffer-filter
       (lambda ()
	 (let ((current (erc-default-target)))
	   (and (stringp current)
		(string-equal downcased-target (erc-downcase current))
		(throw 'buffer (current-buffer)))))
       proc))))

(defun erc-buffer-filter (predicate &optional proc)
  "Return a list of `erc-mode' buffers matching certain criteria.
PREDICATE is a function executed with each buffer, if it returns t, that buffer
is considered a valid match.

PROC is either an `erc-server-process', identifying a certain
server connection, or nil which means all open connections."
  (save-excursion
    (delq
     nil
     (mapcar (lambda (buf)
	       (when (buffer-live-p buf)
		 (with-current-buffer buf
		   (and (eq major-mode 'erc-mode)
			(or (not proc)
			    (eq proc erc-server-process))
			(funcall predicate)
			buf))))
	     (buffer-list)))))

(defun erc-buffer-list (&optional predicate proc)
  "Return a list of ERC buffers.
PREDICATE is a function which executes with every buffer satisfying
the predicate.  If PREDICATE is passed as nil, return a list of all ERC
buffers.  If PROC is given, the buffers local variable `erc-server-process'
needs to match PROC."
  (unless predicate
    (setq predicate (lambda () t)))
  (erc-buffer-filter predicate proc))

(defmacro erc-with-all-buffers-of-server (process pred &rest forms)
  "Execute FORMS in all buffers which have same process as this server.
FORMS will be evaluated in all buffers having the process PROCESS and
where PRED matches or in all buffers of the server process if PRED is
nil."
  ;; Make the evaluation have the correct order
  (let ((pre (make-symbol "pre"))
	(pro (make-symbol "pro")))
    `(let* ((,pro ,process)
	    (,pre ,pred)
	    (res (mapcar (lambda (buffer)
			   (with-current-buffer buffer
			     ,@forms))
			 (erc-buffer-list ,pre
					  ,pro))))
       ;; Silence the byte-compiler by binding the result of mapcar to
       ;; a variable.
       res)))
(put 'erc-with-all-buffers-of-server 'lisp-indent-function 1)
(put 'erc-with-all-buffers-of-server 'edebug-form-spec '(form form body))

;; (iswitchb-mode) will autoload iswitchb.el
(defvar iswitchb-temp-buflist)
(declare-function iswitchb-read-buffer "iswitchb"
		 (prompt &optional default require-match start matches-set))

(defun erc-iswitchb (&optional arg)
  "Use `iswitchb-read-buffer' to prompt for a ERC buffer to switch to.
When invoked with prefix argument, use all erc buffers.  Without prefix
ARG, allow only buffers related to same session server.
If `erc-track-mode' is in enabled, put the last element of
`erc-modified-channels-alist' in front of the buffer list.

Due to some yet unresolved reason, global function `iswitchb-mode'
needs to be active for this function to work."
  (interactive "P")
  (let ((enabled (bound-and-true-p iswitchb-mode)))
    (or enabled (iswitchb-mode 1))
    (unwind-protect
	(let ((iswitchb-make-buflist-hook
	       (lambda ()
		 (setq iswitchb-temp-buflist
		       (mapcar 'buffer-name
			       (erc-buffer-list
				nil
				(when arg erc-server-process)))))))
	  (switch-to-buffer
	   (iswitchb-read-buffer
	    "Switch-to: "
	    (if (boundp 'erc-modified-channels-alist)
		(buffer-name (caar (last erc-modified-channels-alist)))
	      nil)
	    t)))
      (or enabled (iswitchb-mode -1)))))

(defun erc-channel-list (proc)
  "Return a list of channel buffers.
PROC is the process for the server connection.  If PROC is nil, return
all channel buffers on all servers."
  (erc-buffer-filter
   (lambda ()
     (and (erc-default-target)
	  (erc-channel-p (erc-default-target))))
   proc))

(defun erc-buffer-list-with-nick (nick proc)
  "Return buffers containing NICK in the `erc-channel-users' list."
  (with-current-buffer (process-buffer proc)
    (let ((user (gethash (erc-downcase nick) erc-server-users)))
      (if user
	  (erc-server-user-buffers user)
	nil))))

;; Some local variables

(defvar erc-default-recipients nil
  "List of default recipients of the current buffer.")
(make-variable-buffer-local 'erc-default-recipients)

(defvar erc-session-user-full-name nil
  "Full name of the user on the current server.")
(make-variable-buffer-local 'erc-session-user-full-name)

(defvar erc-channel-user-limit nil
  "Limit of users per channel.")
(make-variable-buffer-local 'erc-channel-user-limit)

(defvar erc-channel-key nil
  "Key needed to join channel.")
(make-variable-buffer-local 'erc-channel-key)

(defvar erc-invitation nil
  "Last invitation channel.")
(make-variable-buffer-local 'erc-invitation)

(defvar erc-away nil
  "Non-nil indicates that we are away.

Use `erc-away-time' to access this if you might be in a channel
buffer rather than a server buffer.")
(make-variable-buffer-local 'erc-away)

(defvar erc-channel-list nil
  "Server channel list.")
(make-variable-buffer-local 'erc-channel-list)

(defvar erc-bad-nick nil
  "Non-nil indicates that we got a `nick in use' error while connecting.")
(make-variable-buffer-local 'erc-bad-nick)

(defvar erc-logged-in nil
  "Non-nil indicates that we are logged in.")
(make-variable-buffer-local 'erc-logged-in)

(defvar erc-default-nicks nil
  "The local copy of `erc-nick' - the list of nicks to choose from.")
(make-variable-buffer-local 'erc-default-nicks)

(defvar erc-nick-change-attempt-count 0
  "Used to keep track of how many times an attempt at changing nick is made.")
(make-variable-buffer-local 'erc-nick-change-attempt-count)

(defun erc-migrate-modules (mods)
  "Migrate old names of ERC modules to new ones."
  ;; modify `transforms' to specify what needs to be changed
  ;; each item is in the format '(old . new)
  (let ((transforms '((pcomplete . completion))))
    (erc-delete-dups
     (mapcar (lambda (m) (or (cdr (assoc m transforms)) m))
	     mods))))

(defcustom erc-modules '(netsplit fill button match track completion readonly
			 networks ring autojoin noncommands irccontrols
			 move-to-prompt stamp menu list)
  "A list of modules which ERC should enable.
If you set the value of this without using `customize' remember to call
\(erc-update-modules) after you change it.  When using `customize', modules
removed from the list will be disabled."
  :get (lambda (sym)
	 ;; replace outdated names with their newer equivalents
	 (erc-migrate-modules (symbol-value sym)))
  :set (lambda (sym val)
	 ;; disable modules which have just been removed
	 (when (and (boundp 'erc-modules) erc-modules val)
	   (dolist (module erc-modules)
	     (unless (member module val)
	       (let ((f (intern-soft (format "erc-%s-mode" module))))
		 (when (and (fboundp f) (boundp f) (symbol-value f))
		   (message "Disabling `erc-%s'" module)
		   (funcall f 0))))))
	 (set sym val)
	 ;; this test is for the case where erc hasn't been loaded yet
	 (when (fboundp 'erc-update-modules)
	   (erc-update-modules)))
  :type
  '(set
    :greedy t
    (const :tag "autoaway: Set away status automatically" autoaway)
    (const :tag "autojoin: Join channels automatically" autojoin)
    (const :tag "button: Buttonize URLs, nicknames, and other text" button)
    (const :tag "capab: Mark unidentified users on servers supporting CAPAB"
	   capab-identify)
    (const :tag "completion: Complete nicknames and commands (programmable)"
	   completion)
    (const :tag "hecomplete: Complete nicknames and commands (old)" hecomplete)
    (const :tag "dcc: Provide Direct Client-to-Client support" dcc)
    (const :tag "fill: Wrap long lines" fill)
    (const :tag "identd: Launch an identd server on port 8113" identd)
    (const :tag "irccontrols: Highlight or remove IRC control characters"
	   irccontrols)
    (const :tag "keep-place: Leave point above un-viewed text" keep-place)
    (const :tag "list: List channels in a separate buffer" list)
    (const :tag "log: Save buffers in logs" log)
    (const :tag "match: Highlight pals, fools, and other keywords" match)
    (const :tag "menu: Display a menu in ERC buffers" menu)
    (const :tag "move-to-prompt: Move to the prompt when typing text"
	   move-to-prompt)
    (const :tag "netsplit: Detect netsplits" netsplit)
    (const :tag "networks: Provide data about IRC networks" networks)
    (const :tag "noncommands: Don't display non-IRC commands after evaluation"
	   noncommands)
    (const :tag
	   "notify: Notify when the online status of certain users changes"
	   notify)
    (const :tag "page: Process CTCP PAGE requests from IRC" page)
    (const :tag "readonly: Make displayed lines read-only" readonly)
    (const :tag "replace: Replace text in messages" replace)
    (const :tag "ring: Enable an input history" ring)
    (const :tag "scrolltobottom: Scroll to the bottom of the buffer"
	   scrolltobottom)
    (const :tag "services: Identify to Nickserv (IRC Services) automatically"
	   services)
    (const :tag "smiley: Convert smileys to pretty icons" smiley)
    (const :tag "sound: Play sounds when you receive CTCP SOUND requests"
	   sound)
    (const :tag "stamp: Add timestamps to messages" stamp)
    (const :tag "spelling: Check spelling" spelling)
    (const :tag "track: Track channel activity in the mode-line" track)
    (const :tag "truncate: Truncate buffers to a certain size" truncate)
    (const :tag "unmorse: Translate morse code in messages" unmorse)
    (const :tag "xdcc: Act as an XDCC file-server" xdcc)
    (repeat :tag "Others" :inline t symbol))
  :group 'erc)

(defun erc-update-modules ()
  "Run this to enable erc-foo-mode for all modules in `erc-modules'."
  (let (req)
    (dolist (mod erc-modules)
      (setq req (concat "erc-" (symbol-name mod)))
      (cond
       ;; yuck. perhaps we should bring the filenames into sync?
       ((string= req "erc-capab-identify")
	(setq req "erc-capab"))
       ((string= req "erc-completion")
	(setq req "erc-pcomplete"))
       ((string= req "erc-pcomplete")
	(setq mod 'completion))
       ((string= req "erc-autojoin")
	(setq req "erc-join")))
      (condition-case nil
	  (require (intern req))
	(error nil))
      (let ((sym (intern-soft (concat "erc-" (symbol-name mod) "-mode"))))
	(if (fboundp sym)
	    (funcall sym 1)
	  (error "`%s' is not a known ERC module" mod))))))

(defun erc-setup-buffer (buffer)
  "Consults `erc-join-buffer' to find out how to display `BUFFER'."
  (cond ((eq erc-join-buffer 'window)
	 (if (active-minibuffer-window)
	     (display-buffer buffer)
	   (switch-to-buffer-other-window buffer)))
	((eq erc-join-buffer 'window-noselect)
	 (display-buffer buffer))
	((eq erc-join-buffer 'bury)
	 nil)
	((eq erc-join-buffer 'frame)
	 (when (or (not erc-reuse-frames)
		   (not (get-buffer-window buffer t)))
	   ((lambda (frame)
		     (raise-frame frame)
		     (select-frame frame))
		  (make-frame (or erc-frame-alist
				  default-frame-alist)))
	 (switch-to-buffer buffer)
	 (when erc-frame-dedicated-flag
	   (set-window-dedicated-p (selected-window) t))))
	(t
	 (if (active-minibuffer-window)
	     (display-buffer buffer)
	   (switch-to-buffer buffer)))))

(defun erc-open (&optional server port nick full-name
			   connect passwd tgt-list channel process)
  "Connect to SERVER on PORT as NICK with FULL-NAME.

If CONNECT is non-nil, connect to the server.  Otherwise assume
already connected and just create a separate buffer for the new
target CHANNEL.

Use PASSWD as user password on the server.  If TGT-LIST is
non-nil, use it to initialize `erc-default-recipients'.

Returns the buffer for the given server or channel."
  (let ((server-announced-name (when (and (boundp 'erc-session-server)
					  (string= server erc-session-server))
				 erc-server-announced-name))
	(connected-p (unless connect erc-server-connected))
	(buffer (erc-get-buffer-create server port channel))
	(old-buffer (current-buffer))
	old-point
	continued-session)
    (when connect (run-hook-with-args 'erc-before-connect server port nick))
    (erc-update-modules)
    (set-buffer buffer)
    (setq old-point (point))
    (erc-mode)
    (setq erc-server-announced-name server-announced-name)
    (setq erc-server-connected connected-p)
    ;; connection parameters
    (setq erc-server-process process)
    (setq erc-insert-marker (make-marker))
    (setq erc-input-marker (make-marker))
    ;; go to the end of the buffer and open a new line
    ;; (the buffer may have existed)
    (goto-char (point-max))
    (forward-line 0)
    (when (get-text-property (point) 'erc-prompt)
      (setq continued-session t)
      (set-marker erc-input-marker
		  (or (next-single-property-change (point) 'erc-prompt)
		      (point-max))))
    (unless continued-session
      (goto-char (point-max))
      (insert "\n"))
    (set-marker erc-insert-marker (point))
    ;; stack of default recipients
    (setq erc-default-recipients tgt-list)
    (setq erc-server-current-nick nil)
    ;; Initialize erc-server-users and erc-channel-users
    (if connect
	(progn ;; server buffer
	  (setq erc-server-users
		(make-hash-table :test 'equal))
	  (setq erc-channel-users nil))
      (progn ;; target buffer
	(setq erc-server-users nil)
	(setq erc-channel-users
	      (make-hash-table :test 'equal))))
    ;; clear last incomplete line read
    (setq erc-server-filter-data nil)
    (setq erc-channel-topic "")
    ;; limit on the number of users on the channel (mode +l)
    (setq erc-channel-user-limit nil)
    (setq erc-channel-key nil)
    ;; last active buffer, defaults to this one
    (erc-set-active-buffer buffer)
    ;; last invitation channel
    (setq erc-invitation nil)
    ;; Server channel list
    (setq erc-channel-list ())
    ;; login-time 'nick in use' error
    (setq erc-bad-nick nil)
    ;; whether we have logged in
    (setq erc-logged-in nil)
    ;; The local copy of `erc-nick' - the list of nicks to choose
    (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
    ;; password stuff
    (setq erc-session-password passwd)
    ;; debug output buffer
    (setq erc-dbuf
	  (when erc-log-p
	    (get-buffer-create (concat "*ERC-DEBUG: " server "*"))))
    ;; set up prompt
    (unless continued-session
      (goto-char (point-max))
      (insert "\n"))
    (if continued-session
	(goto-char old-point)
      (set-marker erc-insert-marker (point))
      (erc-display-prompt)
      (goto-char (point-max)))

    (erc-determine-parameters server port nick full-name)

    ;; Saving log file on exit
    (run-hook-with-args 'erc-connect-pre-hook buffer)

    (when connect
      (erc-server-connect erc-session-server erc-session-port buffer))
    (erc-update-mode-line)

    ;; Now display the buffer in a window as per user wishes.
    (unless (eq buffer old-buffer)
      (when erc-log-p
	;; we can't log to debug buffer, it may not exist yet
	(message "erc: old buffer %s, switching to %s"
		 old-buffer buffer))
      (erc-setup-buffer buffer))

    buffer))

(defun erc-initialize-log-marker (buffer)
  "Initialize the `erc-last-saved-position' marker to a sensible position.
BUFFER is the current buffer."
  (with-current-buffer buffer
    (setq erc-last-saved-position (make-marker))
    (move-marker erc-last-saved-position
		 (1- (marker-position erc-insert-marker)))))

;; interactive startup

(defvar erc-server-history-list nil
  "IRC server interactive selection history list.")

(defvar erc-nick-history-list nil
  "Nickname interactive selection history list.")

(defun erc-already-logged-in (server port nick)
  "Return the buffers corresponding to a NICK on PORT of a session SERVER.
This is determined by looking for the appropriate buffer and checking
whether the connection is still alive.
If no buffer matches, return nil."
  (erc-buffer-list
   (lambda ()
     (and (erc-server-process-alive)
	  (string= erc-session-server server)
	  (erc-port-equal erc-session-port port)
	  (erc-current-nick-p nick)))))

(if (not (fboundp 'read-passwd))
    (defun read-passwd (prompt)
      "Substitute for `read-passwd' in early emacsen."
      (read-from-minibuffer prompt)))

(defcustom erc-before-connect nil
  "Hook called before connecting to a server.
This hook gets executed before `erc' actually invokes `erc-mode'
with your input data.  The functions in here get called with three
parameters, SERVER, PORT and NICK."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-after-connect nil
  "Hook called after connecting to a server.
This hook gets executed when an end of MOTD has been received.  All
functions in here get called with the parameters SERVER and NICK."
  :group 'erc-hooks
  :type 'hook)

;;;###autoload
(defun erc-select-read-args ()
  "Prompt the user for values of nick, server, port, and password."
  (let (user-input server port nick passwd)
    (setq user-input (read-from-minibuffer
		      "IRC server: "
		      (erc-compute-server) nil nil 'erc-server-history-list))

    (if (string-match "\\(.*\\):\\(.*\\)\\'" user-input)
	(setq port (erc-string-to-port (match-string 2 user-input))
	      user-input (match-string 1 user-input))
      (setq port
	    (erc-string-to-port (read-from-minibuffer
				 "IRC port: " (erc-port-to-string
					       (erc-compute-port))))))

    (if (string-match "\\`\\(.*\\)@\\(.*\\)" user-input)
	(setq nick (match-string 1 user-input)
	      user-input (match-string 2 user-input))
      (setq nick
	    (if (erc-already-logged-in server port nick)
		(read-from-minibuffer
		 (erc-format-message 'nick-in-use ?n nick)
		 nick
		 nil nil 'erc-nick-history-list)
	      (read-from-minibuffer
	       "Nickname: " (erc-compute-nick nick)
	       nil nil 'erc-nick-history-list))))

    (setq server user-input)

    (setq passwd (if erc-prompt-for-password
		     (if (and erc-password
			      (y-or-n-p "Use the default password? "))
			 erc-password
		       (read-passwd "Password: "))
		   erc-password))
    (when (and passwd (string= "" passwd))
      (setq passwd nil))

    (while (erc-already-logged-in server port nick)
      ;; hmm, this is a problem when using multiple connections to a bnc
      ;; with the same nick. Currently this code prevents using more than one
      ;; bnc with the same nick. actually it would be nice to have
      ;; bncs transparent, so that erc-compute-buffer-name displays
      ;; the server one is connected to.
      (setq nick (read-from-minibuffer
		  (erc-format-message 'nick-in-use ?n nick)
		  nick
		  nil nil 'erc-nick-history-list)))
    (list :server server :port port :nick nick :password passwd)))

;;;###autoload
(defun* erc (&key (server (erc-compute-server))
		  (port   (erc-compute-port))
		  (nick   (erc-compute-nick))
		  password
		  (full-name (erc-compute-full-name)))
  "ERC is a powerful, modular, and extensible IRC client.
This function is the main entry point for ERC.

It permits you to select connection parameters, and then starts ERC.

Non-interactively, it takes the keyword arguments
   (server (erc-compute-server))
   (port   (erc-compute-port))
   (nick   (erc-compute-nick))
   password
   (full-name (erc-compute-full-name)))

That is, if called with

   (erc :server \"irc.freenode.net\" :full-name \"Harry S Truman\")

then the server and full-name will be set to those values, whereas
`erc-compute-port', `erc-compute-nick' and `erc-compute-full-name' will
be invoked for the values of the other parameters."
  (interactive (erc-select-read-args))
  (erc-open server port nick full-name t password))

;;;###autoload
(defalias 'erc-select 'erc)
(defalias 'erc-ssl 'erc-tls)

;;;###autoload
(defun erc-tls (&rest r)
  "Interactively select TLS connection parameters and run ERC.
Arguments are the same as for `erc'."
  (interactive (erc-select-read-args))
  (let ((erc-server-connect-function 'erc-open-tls-stream))
    (apply 'erc r)))

(defun erc-open-tls-stream (name buffer host port)
  "Open an TLS stream to an IRC server.
The process will be given the name NAME, its target buffer will be
BUFFER.  HOST and PORT specify the connection target."
  (open-network-stream name buffer host port
		       :type 'tls))

;;; Displaying error messages

(defun erc-error (&rest args)
  "Pass ARGS to `format', and display the result as an error message.
If `debug-on-error' is set to non-nil, then throw a real error with this
message instead, to make debugging easier."
  (if debug-on-error
      (apply #'error args)
    (apply #'message args)
    (beep)))

;;; Debugging the protocol

(defvar erc-debug-irc-protocol nil
  "If non-nil, log all IRC protocol traffic to the buffer \"*erc-protocol*\".

The buffer is created if it doesn't exist.

NOTE: If this variable is non-nil, and you kill the only
visible \"*erc-protocol*\" buffer, it will be recreated shortly,
but you won't see it.

WARNING: Do not set this variable directly!  Instead, use the
function `erc-toggle-debug-irc-protocol' to toggle its value.")

(declare-function erc-network-name "erc-networks" ())

(defun erc-log-irc-protocol (string &optional outbound)
  "Append STRING to the buffer *erc-protocol*.

This only has any effect if `erc-debug-irc-protocol' is non-nil.

The buffer is created if it doesn't exist.

If OUTBOUND is non-nil, STRING is being sent to the IRC server
and appears in face `erc-input-face' in the buffer."
  (when erc-debug-irc-protocol
    (let ((network-name (or (ignore-errors (erc-network-name))
			    "???")))
      (with-current-buffer (get-buffer-create "*erc-protocol*")
	(save-excursion
	  (goto-char (point-max))
	  (let ((inhibit-read-only t))
	    (insert (if (not outbound)
			;; Cope with the fact that string might
			;; contain multiple lines of text.
			(let ((lines (delete "" (split-string string
							      "\n\\|\r\n")))
			      (result ""))
			  (dolist (line lines)
			    (setq result (concat result network-name
						 " << " line "\n")))
			  result)
		      (erc-propertize
			(concat network-name " >> " string
				(if (/= ?\n
					(aref string
					      (1- (length string))))
				    "\n"))
			'face 'erc-input-face)))))
	(let ((orig-win (selected-window))
	      (debug-buffer-window (get-buffer-window (current-buffer) t)))
	  (when debug-buffer-window
	     (select-window debug-buffer-window)
	     (when (= 1 (count-lines (point) (point-max)))
	       (goto-char (point-max))
	       (recenter -1))
	     (select-window orig-win)))))))

(defun erc-toggle-debug-irc-protocol (&optional arg)
  "Toggle the value of `erc-debug-irc-protocol'.

If ARG is non-nil, show the *erc-protocol* buffer."
  (interactive "P")
  (let* ((buf (get-buffer-create "*erc-protocol*")))
    (with-current-buffer buf
      (erc-view-mode-enter)
      (when (null (current-local-map))
	(let ((inhibit-read-only t))
	  (insert (erc-make-notice "This buffer displays all IRC protocol traffic exchanged with each server.\n"))
	  (insert (erc-make-notice "Kill this buffer to terminate protocol logging.\n\n")))
	(use-local-map (make-sparse-keymap))
	(local-set-key (kbd "t") 'erc-toggle-debug-irc-protocol))
      (add-hook 'kill-buffer-hook
		#'(lambda () (setq erc-debug-irc-protocol nil))
		nil 'local)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert (erc-make-notice
		 (format "IRC protocol logging %s at %s -- Press `t' to toggle logging.\n"
			 (if erc-debug-irc-protocol "disabled" "enabled")
			 (current-time-string))))))
    (setq erc-debug-irc-protocol (not erc-debug-irc-protocol))
    (if (and arg
	     (not (get-buffer-window "*erc-protocol*" t)))
	(display-buffer buf t))
    (message "IRC protocol traffic logging %s (see buffer *erc-protocol*)."
	     (if erc-debug-irc-protocol "enabled" "disabled"))))

;;; I/O interface

;; send interface

(defun erc-send-action (tgt str &optional force)
  "Send CTCP ACTION information described by STR to TGT."
  (erc-send-ctcp-message tgt (format "ACTION %s" str) force)
  (erc-display-message
   nil 'input (current-buffer)
   'ACTION ?n (erc-current-nick) ?a str ?u "" ?h ""))

;; Display interface

(defun erc-string-invisible-p (string)
  "Check whether STRING is invisible or not.
I.e. any char in it has the `invisible' property set."
  (text-property-any 0 (length string) 'invisible t string))

(defcustom erc-remove-parsed-property t
  "Whether to remove the erc-parsed text property after displaying a message.

The default is to remove it, since it causes ERC to take up extra
memory.  If you have code that relies on this property, then set
this option to nil."
  :type 'boolean
  :group 'erc)

(defun erc-display-line-1 (string buffer)
  "Display STRING in `erc-mode' BUFFER.
Auxiliary function used in `erc-display-line'.  The line gets filtered to
interpret the control characters.  Then, `erc-insert-pre-hook' gets called.
If `erc-insert-this' is still t, STRING gets inserted into the buffer.
Afterwards, `erc-insert-modify' and `erc-insert-post-hook' get called.
If STRING is nil, the function does nothing."
  (when string
    (with-current-buffer (or buffer (process-buffer erc-server-process))
      (let ((insert-position (or (marker-position erc-insert-marker)
				 (point-max))))
	(let ((string string) ;; FIXME! Can this be removed?
	      (buffer-undo-list t)
	      (inhibit-read-only t))
	  (unless (string-match "\n$" string)
	    (setq string (concat string "\n"))
	    (when (erc-string-invisible-p string)
	      (erc-put-text-properties 0 (length string)
				       '(invisible intangible) string)))
	  (erc-log (concat "erc-display-line: " string
			   (format "(%S)" string) " in buffer "
			   (format "%s" buffer)))
	  (setq erc-insert-this t)
	  (run-hook-with-args 'erc-insert-pre-hook string)
	  (if (null erc-insert-this)
	      ;; Leave erc-insert-this set to t as much as possible.  Fran
	      ;; Litterio <franl> has seen erc-insert-this set to nil while
	      ;; erc-send-pre-hook is running, which should never happen.  This
	      ;; may cure it.
	      (setq erc-insert-this t)
	    (save-excursion ;; to restore point in the new buffer
	      (save-restriction
		(widen)
		(goto-char insert-position)
		(insert-before-markers string)
		;; run insertion hook, with point at restored location
		(save-restriction
		  (narrow-to-region insert-position (point))
		  (run-hooks 'erc-insert-modify-hook)
		  (run-hooks 'erc-insert-post-hook)
		  (when erc-remove-parsed-property
		    (remove-text-properties (point-min) (point-max)
					    '(erc-parsed nil))))))))
	(erc-update-undo-list (- (or (marker-position erc-insert-marker)
				     (point-max))
				 insert-position))))))

(defun erc-update-undo-list (shift)
  ;; Translate buffer positions in buffer-undo-list by SHIFT.
  (unless (or (zerop shift) (atom buffer-undo-list))
    (let ((list buffer-undo-list) elt)
      (while list
	(setq elt (car list))
	(cond ((integerp elt)		; POSITION
	       (incf (car list) shift))
	      ((or (atom elt)		; nil, EXTENT
		   ;; (eq t (car elt))	; (t . TIME)
		   (markerp (car elt)))	; (MARKER . DISTANCE)
	       nil)
	      ((integerp (car elt))	; (BEGIN . END)
	       (incf (car elt) shift)
	       (incf (cdr elt) shift))
	      ((stringp (car elt))	; (TEXT . POSITION)
	       (incf (cdr elt) (* (if (natnump (cdr elt)) 1 -1) shift)))
	      ((null (car elt))		; (nil PROPERTY VALUE BEG . END)
	       (let ((cons (nthcdr 3 elt)))
		 (incf (car cons) shift)
		 (incf (cdr cons) shift)))
	      ((and (featurep 'xemacs)
		    (extentp (car elt))) ; (EXTENT START END)
	       (incf (nth 1 elt) shift)
	       (incf (nth 2 elt) shift)))
	(setq list (cdr list))))))

(defvar erc-valid-nick-regexp "[]a-zA-Z^[;\\`_{}|][]^[;\\`_{}|a-zA-Z0-9-]*"
  "Regexp which matches all valid characters in a IRC nickname.")

(defun erc-is-valid-nick-p (nick)
  "Check if NICK is a valid IRC nickname."
  (string-match (concat "^" erc-valid-nick-regexp "$") nick))

(defun erc-display-line (string &optional buffer)
  "Display STRING in the ERC BUFFER.
All screen output must be done through this function.  If BUFFER is nil
or omitted, the default ERC buffer for the `erc-session-server' is used.
The BUFFER can be an actual buffer, a list of buffers, 'all or 'active.
If BUFFER = 'all, the string is displayed in all the ERC buffers for the
current session.  'active means the current active buffer
\(`erc-active-buffer').  If the buffer can't be resolved, the current
buffer is used.  `erc-display-line-1' is used to display STRING.

If STRING is nil, the function does nothing."
  (let ((inhibit-point-motion-hooks t)
	new-bufs)
    (dolist (buf (cond
		  ((bufferp buffer) (list buffer))
		  ((listp buffer) buffer)
		  ((processp buffer) (list (process-buffer buffer)))
		  ((eq 'all buffer)
		   ;; Hmm, or all of the same session server?
		   (erc-buffer-list nil erc-server-process))
		  ((and (eq 'active buffer) (erc-active-buffer))
		   (list (erc-active-buffer)))
		  ((erc-server-buffer-live-p)
		   (list (process-buffer erc-server-process)))
		  (t (list (current-buffer)))))
      (when (buffer-live-p buf)
	(erc-display-line-1 string buf)
	(add-to-list 'new-bufs buf)))
    (when (null new-bufs)
      (if (erc-server-buffer-live-p)
	  (erc-display-line-1 string (process-buffer erc-server-process))
	(erc-display-line-1 string (current-buffer))))))

(defun erc-display-message-highlight (type string)
  "Highlight STRING according to TYPE, where erc-TYPE-face is an ERC face.

See also `erc-make-notice'."
  (cond ((eq type 'notice)
	 (erc-make-notice string))
	(t
	 (erc-put-text-property
	  0 (length string)
	  'face (or (intern-soft
		     (concat "erc-" (symbol-name type) "-face"))
		    "erc-default-face")
	  string)
	 string)))

(defun erc-display-message (parsed type buffer msg &rest args)
  "Display MSG in BUFFER.

ARGS, PARSED, and TYPE are used to format MSG sensibly.

See also `erc-format-message' and `erc-display-line'."
  (let ((string (if (symbolp msg)
		    (apply 'erc-format-message msg args)
		  msg)))
    (setq string
	  (cond
	   ((null type)
	    string)
	   ((listp type)
	    (mapc (lambda (type)
		    (setq string
			  (erc-display-message-highlight type string)))
		  type)
	    string)
	   ((symbolp type)
	    (erc-display-message-highlight type string))))

    (if (not (erc-response-p parsed))
	(erc-display-line string buffer)
      (unless (member (erc-response.command parsed) erc-hide-list)
	(erc-put-text-property 0 (length string) 'erc-parsed parsed string)
	(erc-put-text-property 0 (length string) 'rear-sticky t string)
	(erc-display-line string buffer)))))

(defun erc-message-type-member (position list)
  "Return non-nil if the erc-parsed text-property at POSITION is in LIST.

This function relies on the erc-parsed text-property being
present."
  (let ((prop-val (erc-get-parsed-vector position)))
    (and prop-val (member (erc-response.command prop-val) list))))

(defvar erc-send-input-line-function 'erc-send-input-line)
(make-variable-buffer-local 'erc-send-input-line-function)

(defun erc-send-input-line (target line &optional force)
  "Send LINE to TARGET.

See also `erc-server-send'."
  (setq line (format "PRIVMSG %s :%s"
		     target
		     ;; If the line is empty, we still want to
		     ;; send it - i.e. an empty pasted line.
		     (if (string= line "\n")
			 " \n"
		       line)))
  (erc-server-send line force target))

(defun erc-get-arglist (fun)
  "Return the argument list of a function without the parens."
  (let ((arglist (format "%S" (erc-function-arglist fun))))
    (if (string-match "^(\\(.*\\))$" arglist)
	(match-string 1 arglist)
      arglist)))

(defun erc-command-no-process-p (str)
  "Return non-nil if STR is an ERC command that can be run when the process
is not alive, nil otherwise."
  (let ((fun (erc-extract-command-from-line str)))
    (and fun
	 (symbolp (car fun))
	 (get (car fun) 'process-not-needed))))

(defun erc-command-name (cmd)
  "For CMD being the function name of a ERC command, something like
erc-cmd-FOO, this returns a string /FOO."
  (let ((command-name (symbol-name cmd)))
    (if (string-match "^erc-cmd-\\(.*\\)$" command-name)
	(concat "/" (match-string 1 command-name))
      command-name)))

(defun erc-process-input-line (line &optional force no-command)
  "Translate LINE to an RFC1459 command and send it based.
Returns non-nil if the command is actually sent to the server, and nil
otherwise.

If the command in the LINE is not bound as a function `erc-cmd-<COMMAND>',
it is passed to `erc-cmd-default'.  If LINE is not a command (i.e. doesn't
start with /<COMMAND>) then it is sent as a message.

An optional FORCE argument forces sending the line when flood
protection is in effect.  The optional NO-COMMAND argument prohibits
this function from interpreting the line as a command."
  (let ((command-list (erc-extract-command-from-line line)))
    (if	(and command-list
	     (not no-command))
	(let* ((cmd  (nth 0 command-list))
	       (args (nth 1 command-list)))
	  (condition-case nil
	      (if (listp args)
		  (apply cmd args)
		(funcall cmd args))
	    (wrong-number-of-arguments
	     (erc-display-message nil 'error (current-buffer) 'incorrect-args
				  ?c (erc-command-name cmd)
				  ?u (or (erc-get-arglist cmd)
					 "")
				  ?d (format "%s\n"
					     (or (documentation cmd) "")))
	     nil)))
      (let ((r (erc-default-target)))
	(if r
	    (funcall erc-send-input-line-function r line force)
	  (erc-display-message nil 'error (current-buffer) 'no-target)
	  nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		      Input commands handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erc-cmd-AMSG (line)
  "Send LINE to all channels of the current server that you are on."
  (interactive "sSend to all channels you're on: ")
  (setq line (erc-trim-string line))
  (erc-with-all-buffers-of-server nil
   (lambda ()
     (erc-channel-p (erc-default-target)))
   (erc-send-message line)))
(put 'erc-cmd-AMSG 'do-not-parse-args t)

(defun erc-cmd-SAY (line)
  "Send LINE to the current query or channel as a message, not a command.

Use this when you want to send a message with a leading '/'.  Note
that since multi-line messages are never a command, you don't
need this when pasting multiple lines of text."
  (if (string-match "^\\s-*$" line)
      nil
    (string-match "^ ?\\(.*\\)" line)
    (erc-process-input-line (match-string 1 line) nil t)))
(put 'erc-cmd-SAY 'do-not-parse-args t)

(defun erc-cmd-SET (line)
  "Set the variable named by the first word in LINE to some VALUE.
VALUE is computed by evaluating the rest of LINE in Lisp."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-+\\(.*\\)$" line)
    (let ((var (read (concat "erc-" (match-string 1 line))))
	  (val (read (match-string 2 line))))
      (if (boundp var)
	  (progn
	    (set var (eval val))
	    (erc-display-message
	     nil nil 'active (format "Set %S to %S" var val))
	    t)
	(setq var (read (match-string 1 line)))
	(if (boundp var)
	    (progn
	      (set var (eval val))
	      (erc-display-message
	       nil nil 'active (format "Set %S to %S" var val))
	      t)
	  (erc-display-message nil 'error 'active 'variable-not-bound)
	  nil))))
   ((string-match "^\\s-*$" line)
    (erc-display-line
     (concat "Available user variables:\n"
	     (apply
	      'concat
	      (mapcar
	       (lambda (var)
		 (let ((val (symbol-value var)))
		   (concat (format "%S:" var)
			   (if (consp val)
			       (concat "\n" (pp-to-string val))
			     (format " %S\n" val)))))
	       (apropos-internal "^erc-" 'user-variable-p))))
     (current-buffer)) t)
   (t nil)))
(defalias 'erc-cmd-VAR 'erc-cmd-SET)
(defalias 'erc-cmd-VARIABLE 'erc-cmd-SET)
(put 'erc-cmd-SET 'do-not-parse-args t)
(put 'erc-cmd-SET 'process-not-needed t)

(defun erc-cmd-default (line)
  "Fallback command.

Commands for which no erc-cmd-xxx exists, are tunneled through
this function.  LINE is sent to the server verbatim, and
therefore has to contain the command itself as well."
  (erc-log (format "cmd: DEFAULT: %s" line))
  (erc-server-send (substring line 1))
  t)

(defun erc-cmd-IGNORE (&optional user)
  "Ignore USER.  This should be a regexp matching nick!user@host.
If no USER argument is specified, list the contents of `erc-ignore-list'."
  (if user
      (let ((quoted (regexp-quote user)))
	(when (and (not (string= user quoted))
		   (y-or-n-p (format "Use regexp-quoted form (%s) instead? "
				     quoted)))
	  (setq user quoted))
	(erc-display-line
	 (erc-make-notice (format "Now ignoring %s" user))
	 'active)
	(erc-with-server-buffer (add-to-list 'erc-ignore-list user)))
    (if (null (erc-with-server-buffer erc-ignore-list))
	(erc-display-line (erc-make-notice "Ignore list is empty") 'active)
      (erc-display-line (erc-make-notice "Ignore list:") 'active)
      (mapc #'(lambda (item)
		(erc-display-line (erc-make-notice item)
				  'active))
	    (erc-with-server-buffer erc-ignore-list))))
  t)

(defun erc-cmd-UNIGNORE (user)
  "Remove the user specified in USER from the ignore list."
  (let ((ignored-nick (car (erc-with-server-buffer
			     (erc-member-ignore-case (regexp-quote user)
						     erc-ignore-list)))))
    (unless ignored-nick
      (if (setq ignored-nick (erc-ignored-user-p user))
	  (unless (y-or-n-p (format "Remove this regexp (%s)? "
				    ignored-nick))
	    (setq ignored-nick nil))
	(erc-display-line
	 (erc-make-notice (format "%s is not currently ignored!" user))
	 'active)))
    (when ignored-nick
      (erc-display-line
       (erc-make-notice (format "No longer ignoring %s" user))
       'active)
      (erc-with-server-buffer
	(setq erc-ignore-list (delete ignored-nick erc-ignore-list)))))
  t)

(defun erc-cmd-CLEAR ()
  "Clear the window content."
  (recenter 0)
  t)
(put 'erc-cmd-CLEAR 'process-not-needed t)

(defun erc-cmd-OPS ()
  "Show the ops in the current channel."
  (interactive)
  (let ((ops nil))
    (if erc-channel-users
	(maphash (lambda (nick user-data)
		   (let ((cuser (cdr user-data)))
		     (if (and cuser
			      (erc-channel-user-op cuser))
			 (setq ops (cons (erc-server-user-nickname
					  (car user-data))
					 ops)))))
		 erc-channel-users))
    (setq ops (sort ops 'string-lessp))
    (if ops
	(erc-display-message
	 nil 'notice (current-buffer) 'ops
	 ?i (length ops) ?s (if (> (length ops) 1) "s" "")
	 ?o (mapconcat 'identity ops " "))
      (erc-display-message nil 'notice (current-buffer) 'ops-none)))
  t)

(defun erc-cmd-COUNTRY (tld)
  "Display the country associated with the top level domain TLD."
  (require 'mail-extr)
  (let ((co (ignore-errors (what-domain tld))))
    (if co
	(erc-display-message
	 nil 'notice 'active 'country ?c co ?d tld)
      (erc-display-message
       nil 'notice 'active 'country-unknown ?d tld))
  t))
(put 'erc-cmd-COUNTRY 'process-not-needed t)

(defun erc-cmd-AWAY (line)
  "Mark the user as being away, the reason being indicated by LINE.
If no reason is given, unset away status."
  (when (string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (erc-log (format "cmd: AWAY: %s" reason))
      (erc-server-send
       (if (string= reason "")
	   "AWAY"
	 (concat "AWAY :" reason))))
    t))
(put 'erc-cmd-AWAY 'do-not-parse-args t)

(defun erc-cmd-GAWAY (line)
  "Mark the user as being away everywhere, the reason being indicated by LINE."
  ;; on all server buffers.
  (erc-with-all-buffers-of-server nil
    #'erc-open-server-buffer-p
    (erc-cmd-AWAY line)))
(put 'erc-cmd-GAWAY 'do-not-parse-args t)

(defun erc-cmd-CTCP (nick cmd &rest args)
  "Send a Client To Client Protocol message to NICK.

CMD is the CTCP command, possible values being ECHO, FINGER, CLIENTINFO, TIME,
VERSION and so on.  It is called with ARGS."
  (let ((str (concat cmd
		     (when args
		       (concat " " (mapconcat #'identity args " "))))))
    (erc-log (format "cmd: CTCP [%s]: [%s]" nick str))
    (erc-send-ctcp-message nick str)
    t))

(defun erc-cmd-HELP (&optional func)
  "Popup help information.

If FUNC contains a valid function or variable, help about that
will be displayed.  If FUNC is empty, display an apropos about
ERC commands.  Otherwise, do `apropos' in the ERC namespace
\(\"erc-.*LINE\"\).

Examples:
To find out about erc and bbdb, do
  /help bbdb.*

For help about the WHOIS command, do:
  /help whois

For a list of user commands (/join /part, ...):
  /help."
  (if func
    (let* ((sym (or (let ((sym (intern-soft
				(concat "erc-cmd-" (upcase func)))))
		      (if (and sym (or (boundp sym) (fboundp sym)))
			  sym
			nil))
		    (let ((sym (intern-soft func)))
		      (if (and sym (or (boundp sym) (fboundp sym)))
			  sym
			nil))
		    (let ((sym (intern-soft (concat "erc-" func))))
		      (if (and sym (or (boundp sym) (fboundp sym)))
			  sym
			nil)))))
      (if sym
	  (cond
	   ((boundp sym) (describe-variable sym))
	   ((fboundp sym) (describe-function sym))
	   (t nil))
	(apropos-command (concat "erc-.*" func) nil
			 (lambda (x)
			   (or (commandp x)
			       (get x 'custom-type))))
	t))
    (apropos "erc-cmd-")
    (message "Type C-h m to get additional information about keybindings.")
    t))

(defalias 'erc-cmd-H 'erc-cmd-HELP)
(put 'erc-cmd-HELP 'process-not-needed t)

(defun erc-cmd-JOIN (channel &optional key)
  "Join the channel given in CHANNEL, optionally with KEY.
If CHANNEL is specified as \"-invite\", join the channel to which you
were most recently invited.  See also `invitation'."
  (let (chnl)
    (if (string= (upcase channel) "-INVITE")
	(if erc-invitation
	    (setq chnl erc-invitation)
	  (erc-display-message nil 'error (current-buffer) 'no-invitation))
      (setq chnl (erc-ensure-channel-name channel)))
    (when chnl
      ;; Prevent double joining of same channel on same server.
      (let ((joined-channels
	     (mapcar #'(lambda (chanbuf)
			 (with-current-buffer chanbuf (erc-default-target)))
		     (erc-channel-list erc-server-process))))
	(if (erc-member-ignore-case chnl joined-channels)
	    (switch-to-buffer (car (erc-member-ignore-case chnl
							   joined-channels)))
	  (erc-log (format "cmd: JOIN: %s" chnl))
	  (if (and chnl key)
	      (erc-server-send (format "JOIN %s %s" chnl key))
	    (erc-server-send (format "JOIN %s" chnl)))))))
  t)

(defalias 'erc-cmd-CHANNEL 'erc-cmd-JOIN)
(defalias 'erc-cmd-J 'erc-cmd-JOIN)

(defvar erc-channel-new-member-names nil
  "If non-nil, a names list is currently being received.

If non-nil, this variable is a hash-table that associates
received nicks with t.")
(make-variable-buffer-local 'erc-channel-new-member-names)

(defun erc-cmd-NAMES (&optional channel)
  "Display the users in CHANNEL.
If CHANNEL is not specified, display the users in the current channel.
This function clears the channel name list first, then sends the
command."
  (let ((tgt (or (and (erc-channel-p channel) channel)
		 (erc-default-target))))
    (if (and tgt (erc-channel-p tgt))
	(progn
	  (erc-log (format "cmd: DEFAULT: NAMES %s" tgt))
	  (erc-with-buffer
	   (tgt)
	   (erc-channel-begin-receiving-names))
	  (erc-server-send (concat "NAMES " tgt)))
      (erc-display-message nil 'error (current-buffer) 'no-default-channel)))
  t)
(defalias 'erc-cmd-N 'erc-cmd-NAMES)

(defun erc-cmd-KICK (target &optional reason-or-nick &rest reasonwords)
  "Kick the user indicated in LINE from the current channel.
LINE has the format: \"#CHANNEL NICK REASON\" or \"NICK REASON\"."
  (let ((reasonstring (mapconcat 'identity reasonwords " ")))
    (if (string= "" reasonstring)
	(setq reasonstring (format "Kicked by %s" (erc-current-nick))))
    (if (erc-channel-p target)
	(let ((nick reason-or-nick))
	  (erc-log (format "cmd: KICK: %s/%s: %s" nick target reasonstring))
	  (erc-server-send (format "KICK %s %s :%s" target nick reasonstring)
			   nil target)
	  t)
      (when target
	(let ((ch (erc-default-target)))
	  (setq reasonstring (concat
			      (if reason-or-nick (concat reason-or-nick " "))
			      reasonstring))
	  (if ch
	      (progn
		(erc-log
		 (format "cmd: KICK: %s/%s: %s" target ch reasonstring))
		(erc-server-send
		 (format "KICK %s %s :%s" ch target reasonstring) nil ch))
	    (erc-display-message nil 'error (current-buffer)
				 'no-default-channel))
	  t)))))

(defvar erc-script-args nil)

(defun erc-cmd-LOAD (line)
  "Load the script provided in the LINE.
If LINE continues beyond the file name, the rest of
it is put in a (local) variable `erc-script-args',
which can be used in Emacs Lisp scripts.

The optional FORCE argument is ignored here - you can't force loading
a script after exceeding the flood threshold."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(.*\\)$" line)
    (let* ((file-to-find (match-string 1 line))
	   (erc-script-args (match-string 2 line))
	   (file (erc-find-file file-to-find erc-script-path)))
      (erc-log (format "cmd: LOAD: %s" file-to-find))
      (cond
       ((not file)
	(erc-display-message nil 'error (current-buffer)
			     'cannot-find-file ?f file-to-find))
       ((not (file-readable-p file))
	(erc-display-message nil 'error (current-buffer)
			     'cannot-read-file ?f file))
       (t
	(message "Loading \'%s\'..." file)
	(erc-load-script file)
	(message "Loading \'%s\'...done" file))))
    t)
   (t nil)))

(defun erc-cmd-WHOIS (user &optional server)
  "Display whois information for USER.

If SERVER is non-nil, use that, rather than the current server."
  ;; FIXME: is the above docstring correct?  -- Lawrence 2004-01-08
  (let ((send (if server
		  (format "WHOIS %s %s" user server)
		(format "WHOIS %s" user))))
    (erc-log (format "cmd: %s" send))
    (erc-server-send send)
  t))
(defalias 'erc-cmd-WI 'erc-cmd-WHOIS)

(defun erc-cmd-WHOAMI ()
  "Display whois information about yourself."
  (erc-cmd-WHOIS (erc-current-nick))
  t)

(defun erc-cmd-IDLE (nick)
  "Show the length of time NICK has been idle."
  (let ((origbuf (current-buffer))
	symlist)
    (erc-with-server-buffer
      (add-to-list 'symlist
		   (cons (erc-once-with-server-event
			  311 `(string= ,nick
					(second
					 (erc-response.command-args parsed))))
			 'erc-server-311-functions))
      (add-to-list 'symlist
		   (cons (erc-once-with-server-event
			  312 `(string= ,nick
					(second
					 (erc-response.command-args parsed))))
			 'erc-server-312-functions))
      (add-to-list 'symlist
		   (cons (erc-once-with-server-event
			  318 `(string= ,nick
					(second
					 (erc-response.command-args parsed))))
			 'erc-server-318-functions))
      (add-to-list 'symlist
		   (cons (erc-once-with-server-event
			  319 `(string= ,nick
					(second
					 (erc-response.command-args parsed))))
			 'erc-server-319-functions))
      (add-to-list 'symlist
		   (cons (erc-once-with-server-event
			  320 `(string= ,nick
					(second
					 (erc-response.command-args parsed))))
			 'erc-server-320-functions))
      (add-to-list 'symlist
		   (cons (erc-once-with-server-event
			  330 `(string= ,nick
					(second
					 (erc-response.command-args parsed))))
			 'erc-server-330-functions))
      (add-to-list 'symlist
		   (cons (erc-once-with-server-event
			  317
			  `(let ((idleseconds
				  (string-to-number
				   (third
				    (erc-response.command-args parsed)))))
			     (erc-display-line
			      (erc-make-notice
			       (format "%s has been idle for %s."
				       (erc-string-no-properties ,nick)
				       (erc-seconds-to-string idleseconds)))
			      ,origbuf))
			  t)
			 'erc-server-317-functions))

      ;; Send the WHOIS command.
      (erc-cmd-WHOIS nick)

      ;; Remove the uninterned symbols from the server hooks that did not run.
      (run-at-time 20 nil `(lambda ()
			     (with-current-buffer ,(current-buffer)
			       (dolist (sym ',symlist)
				 (let ((hooksym (cdr sym))
				       (funcsym (car sym)))
				   (remove-hook hooksym funcsym t))))))))
  t)

(defun erc-cmd-DESCRIBE (line)
  "Pose some action to a certain user.
LINE has the format \"USER ACTION\"."
  (cond
   ((string-match
     "^\\s-*\\(\\S-+\\)\\s-\\(.*\\)$" line)
    (let ((dst (match-string 1 line))
	  (s (match-string 2 line)))
      (erc-log (format "cmd: DESCRIBE: [%s] %s" dst s))
      (erc-send-action dst s))
    t)
   (t nil)))
(put 'erc-cmd-DESCRIBE 'do-not-parse-args t)

(defun erc-cmd-ME (line)
  "Send LINE as an action."
  (cond
   ((string-match "^\\s-\\(.*\\)$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: ME: %s" s))
      (erc-send-action (erc-default-target) s))
    t)
   (t nil)))
(put 'erc-cmd-ME 'do-not-parse-args t)

(defun erc-cmd-ME\'S (line)
  "Do a /ME command, but add the string \" 's\" to the beginning."
  (erc-cmd-ME (concat " 's" line)))
(put 'erc-cmd-ME\'S 'do-not-parse-args t)

(defun erc-cmd-LASTLOG (line)
  "Show all lines in the current buffer matching the regexp LINE.

If a match spreads across multiple lines, all those lines are shown.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how.

If LINE contains upper case characters (excluding those preceded by `\'),
the matching is case-sensitive."
  (occur line)
  t)
(put 'erc-cmd-LASTLOG 'do-not-parse-args t)
(put 'erc-cmd-LASTLOG 'process-not-needed t)

(defun erc-send-message (line &optional force)
  "Send LINE to the current channel or user and display it.

See also `erc-message' and `erc-display-line'."
  (erc-message "PRIVMSG" (concat (erc-default-target) " " line) force)
  (erc-display-line
   (concat (erc-format-my-nick) line)
     (current-buffer))
  ;; FIXME - treat multiline, run hooks, or remove me?
  t)

(defun erc-cmd-MODE (line)
  "Change or display the mode value of a channel or user.
The first word specifies the target.  The rest is the mode string
to send.

If only one word is given, display the mode of that target.

A list of valid mode strings for Freenode may be found at
URL `http://freenode.net/using_the_network.shtml'."
  (cond
   ((string-match "^\\s-\\(.*\\)$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: MODE: %s" s))
      (erc-server-send (concat "MODE " line)))
    t)
   (t nil)))
(put 'erc-cmd-MODE 'do-not-parse-args t)

(defun erc-cmd-NOTICE (channel-or-user &rest message)
  "Send a notice to the channel or user given as the first word.
The rest is the message to send."
  (erc-message "NOTICE" (concat channel-or-user " "
				(mapconcat #'identity message " "))))

(defun erc-cmd-MSG (line)
  "Send a message to the channel or user given as the first word in LINE.

The rest of LINE is the message to send."
  (erc-message "PRIVMSG" line))

(defalias 'erc-cmd-M 'erc-cmd-MSG)
(put 'erc-cmd-MSG 'do-not-parse-args t)

(defun erc-cmd-SQUERY (line)
  "Send a Service Query to the service given as the first word in LINE.

The rest of LINE is the message to send."
  (erc-message "SQUERY" line))

(defun erc-cmd-NICK (nick)
  "Change current nickname to NICK."
  (erc-log (format "cmd: NICK: %s (erc-bad-nick: %S)" nick erc-bad-nick))
  (let ((nicklen (cdr (assoc "NICKLEN" (erc-with-server-buffer
					 erc-server-parameters)))))
    (and nicklen (> (length nick) (string-to-number nicklen))
	 (erc-display-message
	  nil 'notice 'active 'nick-too-long
	  ?i (length nick) ?l nicklen)))
  (erc-server-send (format "NICK %s" nick))
  (cond (erc-bad-nick
	 (erc-set-current-nick nick)
	 (erc-update-mode-line)
	 (setq erc-bad-nick nil)))
  t)

(defun erc-cmd-PART (line)
  "When LINE is an empty string, leave the current channel.
Otherwise leave the channel indicated by LINE."
  (cond
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)\\s-?\\(.*\\)$" line)
    (let* ((ch (match-string 1 line))
	   (msg (match-string 2 line))
	   (reason (funcall erc-part-reason (if (equal msg "") nil msg))))
      (erc-log (format "cmd: PART: %s: %s" ch reason))
      (erc-server-send (if (string= reason "")
			   (format "PART %s" ch)
			 (format "PART %s :%s" ch reason))
		       nil ch))
    t)
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let* ((ch (erc-default-target))
	   (msg (match-string 1 line))
	   (reason (funcall erc-part-reason (if (equal msg "") nil msg))))
      (if (and ch (erc-channel-p ch))
	  (progn
	    (erc-log (format "cmd: PART: %s: %s" ch reason))
	    (erc-server-send (if (string= reason "")
				 (format "PART %s" ch)
			       (format "PART %s :%s" ch reason))
			     nil ch))
	(erc-display-message nil 'error (current-buffer) 'no-target)))
    t)
   (t nil)))
(put 'erc-cmd-PART 'do-not-parse-args t)

(defalias 'erc-cmd-LEAVE 'erc-cmd-PART)

(defun erc-cmd-PING (recipient)
  "Ping RECIPIENT."
  (let ((time (format "%f" (erc-current-time))))
    (erc-log (format "cmd: PING: %s" time))
    (erc-cmd-CTCP recipient "PING" time)))

(defun erc-cmd-QUOTE (line)
  "Send LINE directly to the server.
All the text given as argument is sent to the sever as unmodified,
just as you provided it.  Use this command with care!"
  (cond
   ((string-match "^ ?\\(.+\\)$" line)
    (erc-server-send (match-string 1 line)))
   (t nil)))
(put 'erc-cmd-QUOTE 'do-not-parse-args t)

(defcustom erc-query-display 'window
  "Indicates how to display query buffers when using the /QUERY
command to talk to someone.

The default behavior is to display the message in a new window
and bring it to the front.  See the documentation for
`erc-join-buffer' for a description of the available choices.

See also `erc-auto-query' to decide how private messages from
other people should be displayed."
  :group 'erc-query
  :type '(choice (const :tag "Split window and select" window)
		 (const :tag "Split window, don't select" window-noselect)
		 (const :tag "New frame" frame)
		 (const :tag "Bury in new buffer" bury)
		 (const :tag "Use current buffer" buffer)
		 (const :tag "Use current buffer" t)))

(defun erc-cmd-QUERY (&optional user)
  "Open a query with USER.
The type of query window/frame/etc will depend on the value of
`erc-query-display'.

If USER is omitted, close the current query buffer if one exists
- except this is broken now ;-)"
  (interactive
   (list (read-from-minibuffer "Start a query with: " nil)))
  (let ((session-buffer (erc-server-buffer))
	(erc-join-buffer erc-query-display))
    (if user
	(erc-query user session-buffer)
      ;; currently broken, evil hack to display help anyway
      ;(erc-delete-query))))
      (signal 'wrong-number-of-arguments ""))))
(defalias 'erc-cmd-Q 'erc-cmd-QUERY)

(defun erc-quit-reason-normal (&optional s)
  "Normal quit message.

If S is non-nil, it will be used as the quit reason."
  (or s
      (format "\C-bERC\C-b %s (IRC client for Emacs)"; - \C-b%s\C-b"
	      erc-version-string) ; erc-official-location)
  ))

(defun erc-quit-reason-zippy (&optional s)
  "Zippy quit message.

If S is non-nil, it will be used as the quit reason."
  (or s
      (erc-replace-regexp-in-string "\n" "" (yow))))

(defun erc-quit-reason-various (s)
  "Choose a quit reason based on S (a string)."
  (when (featurep 'xemacs) (require 'poe))
  (let ((res (car (assoc-default (or s "")
		   erc-quit-reason-various-alist 'string-match))))
    (cond
     ((functionp res) (funcall res))
     ((stringp res) res)
     (s s)
     (t (erc-quit-reason-normal)))))

(defun erc-part-reason-normal (&optional s)
  "Normal part message.

If S is non-nil, it will be used as the quit reason."
  (or s
      (format "\C-bERC\C-b %s (IRC client for Emacs)"; - \C-b%s\C-b"
	      erc-version-string) ; erc-official-location)
  ))

(defun erc-part-reason-zippy (&optional s)
  "Zippy part message.

If S is non-nil, it will be used as the quit reason."
  (or s
      (erc-replace-regexp-in-string "\n" "" (yow))))

(defun erc-part-reason-various (s)
  "Choose a part reason based on S (a string)."
  (when (featurep 'xemacs) (require 'poe))
  (let ((res (car (assoc-default (or s "")
		   erc-part-reason-various-alist 'string-match))))
    (cond
     ((functionp res) (funcall res))
     ((stringp res) res)
     (s s)
     (t (erc-part-reason-normal)))))

(defun erc-cmd-QUIT (reason)
  "Disconnect from the current server.
If REASON is omitted, display a default quit message, otherwise display
the message given by REASON."
  (unless reason
    (setq reason ""))
  (cond
   ((string-match "^\\s-*\\(.*\\)$" reason)
    (let* ((s (match-string 1 reason))
	   (buffer (erc-server-buffer))
	   (reason (funcall erc-quit-reason (if (equal s "") nil s)))
	   server-proc)
      (with-current-buffer (if (and buffer
				    (bufferp buffer))
			       buffer
			     (current-buffer))
	(erc-log (format "cmd: QUIT: %s" reason))
	(setq erc-server-quitting t)
	(erc-set-active-buffer (erc-server-buffer))
	(setq server-proc erc-server-process)
	(erc-server-send (format "QUIT :%s" reason)))
      (run-hook-with-args 'erc-quit-hook server-proc)
      (when erc-kill-queries-on-quit
	(erc-kill-query-buffers server-proc))
      ;; if the process has not been killed within 4 seconds, kill it
      (run-at-time 4 nil
		   (lambda (proc)
		     (when (and (processp proc)
				(memq (process-status proc) '(run open)))
		       (delete-process proc)))
		   server-proc))
    t)
   (t nil)))

(defalias 'erc-cmd-BYE 'erc-cmd-QUIT)
(defalias 'erc-cmd-EXIT 'erc-cmd-QUIT)
(defalias 'erc-cmd-SIGNOFF 'erc-cmd-QUIT)
(put 'erc-cmd-QUIT 'do-not-parse-args t)
(put 'erc-cmd-QUIT 'process-not-needed t)

(defun erc-cmd-GQUIT (reason)
  "Disconnect from all servers at once with the same quit REASON."
  (erc-with-all-buffers-of-server nil #'erc-open-server-buffer-p
				  (erc-cmd-QUIT reason))
  (when erc-kill-queries-on-quit
    ;; if the query buffers have not been killed within 4 seconds,
    ;; kill them
    (run-at-time
     4 nil
     (lambda ()
       (dolist (buffer (erc-buffer-list (lambda (buf)
					  (not (erc-server-buffer-p buf)))))
	 (kill-buffer buffer)))))
  t)

(defalias 'erc-cmd-GQ 'erc-cmd-GQUIT)
(put 'erc-cmd-GQUIT 'do-not-parse-args t)
(put 'erc-cmd-GQUIT 'process-not-needed t)

(defun erc-cmd-RECONNECT ()
  "Try to reconnect to the current IRC server."
  (let ((buffer (erc-server-buffer))
	(process nil))
    (unless (buffer-live-p buffer)
      (setq buffer (current-buffer)))
    (with-current-buffer buffer
      (setq erc-server-quitting nil)
      (setq erc-server-reconnecting t)
      (setq erc-server-reconnect-count 0)
      (setq process (get-buffer-process (erc-server-buffer)))
      (if process
	  (delete-process process)
	(erc-server-reconnect))
      (setq erc-server-reconnecting nil)))
  t)
(put 'erc-cmd-RECONNECT 'process-not-needed t)

(defun erc-cmd-SERVER (server)
  "Connect to SERVER, leaving existing connection intact."
  (erc-log (format "cmd: SERVER: %s" server))
  (condition-case nil
      (erc :server server :nick (erc-current-nick))
    (error
     (erc-error "Cannot find host %s." server)))
  t)
(put 'erc-cmd-SERVER 'process-not-needed t)

(defvar motif-version-string)
(defvar gtk-version-string)

(defun erc-cmd-SV ()
  "Say the current ERC and Emacs version into channel."
  (erc-send-message (format "I'm using ERC %s with %s %s (%s%s) of %s."
			    erc-version-string
			    (if (featurep 'xemacs) "XEmacs" "GNU Emacs")
			    emacs-version
			    system-configuration
			    (concat
			     (cond ((featurep 'motif)
				    (concat ", " (substring
						  motif-version-string 4)))
				   ((featurep 'gtk)
				    (concat ", GTK+ Version "
					    gtk-version-string))
				   ((featurep 'x-toolkit) ", X toolkit")
				   (t ""))
			     (if (and (boundp 'x-toolkit-scroll-bars)
				      (memq x-toolkit-scroll-bars
					    '(xaw xaw3d)))
				 (format ", %s scroll bars"
					 (capitalize (symbol-name
						      x-toolkit-scroll-bars)))
			       "")
			     (if (featurep 'multi-tty) ", multi-tty" ""))
			    erc-emacs-build-time))
  t)

(defun erc-cmd-SM ()
  "Say the current ERC modes into channel."
  (erc-send-message (format "I'm using the following modules: %s!"
			    (erc-modes)))
  t)

(defun erc-cmd-DEOP (&rest people)
  "Remove the operator setting from user(s) given in PEOPLE."
  (when (> (length people) 0)
    (erc-server-send (concat "MODE " (erc-default-target)
			      " -"
			      (make-string (length people) ?o)
			      " "
			      (mapconcat 'identity people " ")))
    t))

(defun erc-cmd-OP (&rest people)
  "Add the operator setting to users(s) given in PEOPLE."
  (when (> (length people) 0)
    (erc-server-send (concat "MODE " (erc-default-target)
			      " +"
			      (make-string (length people) ?o)
			      " "
			      (mapconcat 'identity people " ")))
    t))

(defun erc-cmd-TIME (&optional line)
  "Request the current time and date from the current server."
  (cond
   ((and line (string-match "^\\s-*\\(.*\\)$" line))
    (let ((args (match-string 1 line)))
      (erc-log (format "cmd: TIME: %s" args))
      (erc-server-send (concat "TIME " args)))
    t)
   (t (erc-server-send "TIME"))))
(defalias 'erc-cmd-DATE 'erc-cmd-TIME)

(defun erc-cmd-TOPIC (topic)
  "Set or request the topic for a channel.
LINE has the format: \"#CHANNEL TOPIC\", \"#CHANNEL\", \"TOPIC\"
or the empty string.

If no #CHANNEL is given, the default channel is used.  If TOPIC is
given, the channel topic is modified, otherwise the current topic will
be displayed."
  (cond
   ;; /topic #channel TOPIC
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)\\s-\\(.*\\)$" topic)
    (let ((ch (match-string 1 topic))
	  (topic (match-string 2 topic)))
      (erc-log (format "cmd: TOPIC [%s]: %s" ch topic))
      (erc-server-send (format "TOPIC %s :%s" ch topic) nil ch))
    t)
   ;; /topic #channel
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)" topic)
    (let ((ch (match-string 1 topic)))
      (erc-server-send (format "TOPIC %s" ch) nil ch)
      t))
   ;; /topic
   ((string-match "^\\s-*$" topic)
    (let ((ch (erc-default-target)))
      (erc-server-send (format "TOPIC %s" ch) nil ch)
      t))
   ;; /topic TOPIC
   ((string-match "^\\s-*\\(.*\\)$" topic)
    (let ((ch (erc-default-target))
	  (topic (match-string 1 topic)))
      (if (and ch (erc-channel-p ch))
	  (progn
	    (erc-log (format "cmd: TOPIC [%s]: %s" ch topic))
	    (erc-server-send (format "TOPIC %s :%s" ch topic) nil ch))
	(erc-display-message nil 'error (current-buffer) 'no-target)))
    t)
   (t nil)))
(defalias 'erc-cmd-T 'erc-cmd-TOPIC)
(put 'erc-cmd-TOPIC 'do-not-parse-args t)

(defun erc-cmd-APPENDTOPIC (topic)
  "Append TOPIC to the current channel topic, separated by a space."
  (let ((oldtopic erc-channel-topic))
    ;; display help when given no arguments
    (when (string-match "^\\s-*$" topic)
      (signal 'wrong-number-of-arguments nil))
    ;; strip trailing ^O
    (when (string-match "\\(.*\\)\C-o" oldtopic)
      (erc-cmd-TOPIC (concat (match-string 1 oldtopic) topic)))))
(defalias 'erc-cmd-AT 'erc-cmd-APPENDTOPIC)
(put 'erc-cmd-APPENDTOPIC 'do-not-parse-args t)

(defun erc-cmd-CLEARTOPIC (&optional channel)
  "Clear the topic for a CHANNEL.
If CHANNEL is not specified, clear the topic for the default channel."
  (interactive "sClear topic of channel (RET is current channel): ")
  (let ((chnl (or (and (erc-channel-p channel) channel) (erc-default-target))))
    (when chnl
      (erc-server-send (format "TOPIC %s :" chnl))
      t)))

;;; Banlists

(defvar erc-channel-banlist nil
  "A list of bans seen for the current channel.

Each ban is an alist of the form:
  (WHOSET . MASK)

The property `received-from-server' indicates whether
or not the ban list has been requested from the server.")
(make-variable-buffer-local 'erc-channel-banlist)
(put 'erc-channel-banlist 'received-from-server nil)

(defun erc-cmd-BANLIST ()
  "Pretty-print the contents of `erc-channel-banlist'.

The ban list is fetched from the server if necessary."
  (let ((chnl (erc-default-target))
	(chnl-name (buffer-name)))

    (cond
     ((not (erc-channel-p chnl))
      (erc-display-line (erc-make-notice "You're not on a channel\n")
			'active))

     ((not (get 'erc-channel-banlist 'received-from-server))
      (let ((old-367-hook erc-server-367-functions))
	(setq erc-server-367-functions 'erc-banlist-store
	      erc-channel-banlist nil)
	;; fetch the ban list then callback
	(erc-with-server-buffer
	  (erc-once-with-server-event
	   368
	   `(with-current-buffer ,chnl-name
	      (put 'erc-channel-banlist 'received-from-server t)
	      (setq erc-server-367-functions ',old-367-hook)
	      (erc-cmd-BANLIST)
	      t))
	  (erc-server-send (format "MODE %s b" chnl)))))

     ((null erc-channel-banlist)
      (erc-display-line (erc-make-notice
			 (format "No bans for channel: %s\n" chnl))
			'active)
      (put 'erc-channel-banlist 'received-from-server nil))

     (t
      (let* ((erc-fill-column (or (and (boundp 'erc-fill-column)
				       erc-fill-column)
				  (and (boundp 'fill-column)
				       fill-column)
				  (1- (window-width))))
	     (separator (make-string erc-fill-column ?=))
	     (fmt (concat
		   "%-" (number-to-string (/ erc-fill-column 2)) "s"
		   "%" (number-to-string (/ erc-fill-column 2)) "s")))

	(erc-display-line
	 (erc-make-notice (format "Ban list for channel: %s\n"
				  (erc-default-target)))
	 'active)

	(erc-display-line separator 'active)
	(erc-display-line (format fmt "Ban Mask" "Banned By") 'active)
	(erc-display-line separator 'active)

	(mapc
	 (lambda (x)
	   (erc-display-line
	    (format fmt
		    (truncate-string-to-width (cdr x) (/ erc-fill-column 2))
		    (if (car x)
			(truncate-string-to-width (car x) (/ erc-fill-column 2))
		      ""))
	    'active))
	 erc-channel-banlist)

	(erc-display-line (erc-make-notice "End of Ban list")
			  'active)
	(put 'erc-channel-banlist 'received-from-server nil)))))
  t)

(defalias 'erc-cmd-BL 'erc-cmd-BANLIST)

(defun erc-cmd-MASSUNBAN ()
  "Mass Unban.

Unban all currently banned users in the current channel."
  (let ((chnl (erc-default-target)))
    (cond

     ((not (erc-channel-p chnl))
      (erc-display-line
       (erc-make-notice "You're not on a channel\n")
       'active))

     ((not (get 'erc-channel-banlist 'received-from-server))
      (let ((old-367-hook erc-server-367-functions))
	(setq erc-server-367-functions 'erc-banlist-store)
      ;; fetch the ban list then callback
      (erc-with-server-buffer
	(erc-once-with-server-event
	 368
	 `(with-current-buffer ,chnl
	    (put 'erc-channel-banlist 'received-from-server t)
	      (setq erc-server-367-functions ,old-367-hook)
	    (erc-cmd-MASSUNBAN)
	    t))
	  (erc-server-send (format "MODE %s b" chnl)))))

     (t (let ((bans (mapcar 'cdr erc-channel-banlist)))
    (when bans
      ;; Glob the bans into groups of three, and carry out the unban.
      ;; eg. /mode #foo -bbb a*!*@* b*!*@* c*!*@*
      (mapc
       (lambda (x)
	 (erc-server-send
	  (format "MODE %s -%s %s" (erc-default-target)
		  (make-string (length x) ?b)
			(mapconcat 'identity x " "))))
       (erc-group-list bans 3))))
	t))))

(defalias 'erc-cmd-MUB 'erc-cmd-MASSUNBAN)

;;;; End of IRC commands

(defun erc-ensure-channel-name (channel)
  "Return CHANNEL if it is a valid channel name.
Eventually add a # in front of it, if that turns it into a valid channel name."
  (if (erc-channel-p channel)
      channel
    (concat "#" channel)))

(defun erc-grab-region (start end)
  "Copy the region between START and END in a recreatable format.

Converts all the IRC text properties in each line of the region
into control codes and writes them to a separate buffer.  The
resulting text may be used directly as a script to generate this
text again."
  (interactive "r")
  (erc-set-active-buffer (current-buffer))
  (save-excursion
    (let* ((cb (current-buffer))
	   (buf (generate-new-buffer erc-grab-buffer-name))
	   (region (buffer-substring start end))
	   (lines (erc-split-multiline-safe region)))
      (set-buffer buf)
      (dolist (line lines)
	(insert (concat line "\n")))
      (set-buffer cb)
      (switch-to-buffer-other-window buf)))
  (message "erc-grab-region doesn't grab colors etc. anymore. If you use this, please tell the maintainers.")
  (ding))

(defun erc-display-prompt (&optional buffer pos prompt face)
  "Display PROMPT in BUFFER at position POS.
Display an ERC prompt in BUFFER.

If PROMPT is nil, one is constructed with the function `erc-prompt'.
If BUFFER is nil, the `current-buffer' is used.
If POS is nil, PROMPT will be displayed at `point'.
If FACE is non-nil, it will be used to propertize the prompt.  If it is nil,
`erc-prompt-face' will be used."
  (let* ((prompt (or prompt (erc-prompt)))
	 (l (length prompt))
	 (ob (current-buffer)))
    ;; We cannot use save-excursion because we move point, therefore
    ;; we resort to the ol' ob trick to restore this.
    (when (and buffer (bufferp buffer))
      (set-buffer buffer))

    ;; now save excursion again to store where point and mark are
    ;; in the current buffer
    (save-excursion
      (setq pos (or pos (point)))
      (goto-char pos)
      (when (> l 0)
	;; Do not extend the text properties when typing at the end
	;; of the prompt, but stuff typed in front of the prompt
	;; shall remain part of the prompt.
	(setq prompt (erc-propertize prompt
				     'start-open t ; XEmacs
				     'rear-nonsticky t ; Emacs
				     'erc-prompt t
				     'front-sticky t
				     'read-only t))
	(erc-put-text-property 0 (1- (length prompt))
			       'face (or face 'erc-prompt-face)
			       prompt)
	(insert prompt))
      ;; Set the input marker
      (set-marker erc-input-marker (point)))

    ;; Now we are back at the old position.  If the prompt was
    ;; inserted here or before us, advance point by the length of
    ;; the prompt.
    (when (or (not pos) (<= (point) pos))
      (forward-char l))
    ;; Clear the undo buffer now, so the user can undo his stuff,
    ;; but not the stuff we did. Sneaky!
    (setq buffer-undo-list nil)
    (set-buffer ob)))

;; interactive operations

(defun erc-input-message ()
  "Read input from the minibuffer."
  (interactive)
  (let ((minibuffer-allow-text-properties t)
	(read-map minibuffer-local-map))
    (insert (read-from-minibuffer "Message: "
				  (string (if (featurep 'xemacs)
					      last-command-char
					    last-command-event)) read-map))
    (erc-send-current-line)))

(defvar erc-action-history-list ()
  "History list for interactive action input.")

(defun erc-input-action ()
  "Interactively input a user action and send it to IRC."
  (interactive "")
  (erc-set-active-buffer (current-buffer))
  (let ((action (read-from-minibuffer
		 "Action: " nil nil nil 'erc-action-history-list)))
    (if (not (string-match "^\\s-*$" action))
	(erc-send-action (erc-default-target) action))))

(defun erc-join-channel (channel &optional key)
  "Join CHANNEL.

If `point' is at the beginning of a channel name, use that as default."
  (interactive
   (list
    (let ((chnl (if (looking-at "\\([&#+!][^ \n]+\\)") (match-string 1) ""))
	  (table (when (erc-server-buffer-live-p)
		   (set-buffer (process-buffer erc-server-process))
		   erc-channel-list)))
      (completing-read "Join channel: " table nil nil nil nil chnl))
    (when (or current-prefix-arg erc-prompt-for-channel-key)
      (read-from-minibuffer "Channel key (RET for none): " nil))))
  (erc-cmd-JOIN channel (when (>= (length key) 1) key)))

(defun erc-part-from-channel (reason)
  "Part from the current channel and prompt for a REASON."
  (interactive
   (list
    (if (and (boundp 'reason) (stringp reason) (not (string= reason "")))
	reason
      (read-from-minibuffer (concat "Reason for leaving " (erc-default-target)
				    ": ")))))
  (erc-cmd-PART (concat (erc-default-target)" " reason)))

(defun erc-set-topic (topic)
  "Prompt for a TOPIC for the current channel."
  (interactive
   (list
    (read-from-minibuffer
     (concat "Set topic of " (erc-default-target) ": ")
     (when erc-channel-topic
       (let ((ss (split-string erc-channel-topic "\C-o")))
	 (cons (apply 'concat (if (cdr ss) (butlast ss) ss))
	       0))))))
  (let ((topic-list (split-string topic "\C-o"))) ; strip off the topic setter
    (erc-cmd-TOPIC (concat (erc-default-target) " " (car topic-list)))))

(defun erc-set-channel-limit (&optional limit)
  "Set a LIMIT for the current channel.  Remove limit if nil.
Prompt for one if called interactively."
  (interactive (list (read-from-minibuffer
		      (format "Limit for %s (RET to remove limit): "
			      (erc-default-target)))))
  (let ((tgt (erc-default-target)))
    (if (and limit (>= (length limit) 1))
	(erc-server-send (format "MODE %s +l %s" tgt limit))
      (erc-server-send (format "MODE %s -l" tgt)))))

(defun erc-set-channel-key (&optional key)
  "Set a KEY for the current channel.  Remove key if nil.
Prompt for one if called interactively."
  (interactive (list (read-from-minibuffer
		      (format "Key for %s (RET to remove key): "
			      (erc-default-target)))))
  (let ((tgt (erc-default-target)))
    (if (and key (>= (length key) 1))
	(erc-server-send (format "MODE %s +k %s" tgt key))
      (erc-server-send (format "MODE %s -k" tgt)))))

(defun erc-quit-server (reason)
  "Disconnect from current server after prompting for REASON.
`erc-quit-reason' works with this just like with `erc-cmd-QUIT'."
  (interactive (list (read-from-minibuffer
		      (format "Reason for quitting %s: "
			      (or erc-server-announced-name
				  erc-session-server)))))
  (erc-cmd-QUIT reason))

;; Movement of point

(defun erc-bol ()
  "Move `point' to the beginning of the current line.

This places `point' just after the prompt, or at the beginning of the line."
  (interactive)
  (forward-line 0)
  (when (get-text-property (point) 'erc-prompt)
    (goto-char erc-input-marker))
  (point))

(defun erc-kill-input ()
  "Kill current input line using `erc-bol' followed by `kill-line'."
  (interactive)
  (when (and (erc-bol)
	     (/= (point) (point-max))) ;; Prevent a (ding) and an error when
				       ;; there's nothing to kill
    (if (boundp 'erc-input-ring-index)
	(setq erc-input-ring-index nil))
    (kill-line)))

(defun erc-complete-word-at-point ()
  (run-hook-with-args-until-success 'erc-complete-functions))

(define-obsolete-function-alias 'erc-complete-word 'completion-at-point "24.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;			  IRC SERVER INPUT HANDLING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; New Input parsing

; Stolen from ZenIRC. I just wanna test this code, so here is
; experiment area.

(defcustom erc-default-server-hook '(erc-debug-missing-hooks
				     erc-default-server-handler)
  "*Default for server messages which aren't covered by `erc-server-hooks'."
  :group 'erc-server-hooks
  :type 'hook)

(defun erc-default-server-handler (proc parsed)
  "Default server handler.

Displays PROC and PARSED appropriately using `erc-display-message'."
  (erc-display-message
   parsed 'notice proc
   (mapconcat
    'identity
    (let (res)
      (mapc #'(lambda (x)
		(if (stringp x)
		    (setq res (append res (list x)))))
	    parsed)
      res)
    " ")))

(defvar erc-server-vectors
  '(["msgtype" "sender" "to" "arg1" "arg2" "arg3" "..."])
  "List of received server messages which ERC does not specifically handle.
See `erc-debug-missing-hooks'.")
;(make-variable-buffer-local 'erc-server-vectors)

(defun erc-debug-missing-hooks (proc parsed)
  "Add PARSED server message ERC does not yet handle to `erc-server-vectors'.
These vectors can be helpful when adding new server message handlers to ERC.
See `erc-default-server-hook'."
  (nconc erc-server-vectors (list parsed))
  nil)

(defun erc-query (target server)
  "Open a query buffer on TARGET, using SERVER.
To change how this query window is displayed, use `let' to bind
`erc-join-buffer' before calling this."
  (unless (and server
	       (buffer-live-p server)
	       (set-buffer server))
    (error "Couldn't switch to server buffer"))
  (let ((buf (erc-open erc-session-server
		       erc-session-port
		       (erc-current-nick)
		       erc-session-user-full-name
		       nil
		       nil
		       (list target)
		       target
		       erc-server-process)))
    (unless buf
      (error "Couldn't open query window"))
    (erc-update-mode-line)
    buf))

(defcustom erc-auto-query 'window-noselect
  "If non-nil, create a query buffer each time you receive a private message.
If the buffer doesn't already exist, it is created.

This can be set to a symbol, to control how the new query window
should appear.  The default behavior is to display the buffer in
a new window, but not to select it.  See the documentation for
`erc-join-buffer' for a description of the available choices."
  :group 'erc-query
  :type '(choice (const :tag "Don't create query window" nil)
		 (const :tag "Split window and select" window)
		 (const :tag "Split window, don't select" window-noselect)
		 (const :tag "New frame" frame)
		 (const :tag "Bury in new buffer" bury)
		 (const :tag "Use current buffer" buffer)
		 (const :tag "Use current buffer" t)))

(defcustom erc-query-on-unjoined-chan-privmsg t
  "If non-nil create query buffer on receiving any PRIVMSG at all.
This includes PRIVMSGs directed to channels.  If you are using an IRC
bouncer, such as dircproxy, to keep a log of channels when you are
disconnected, you should set this option to t."
  :group 'erc-query
  :type 'boolean)

(defcustom erc-format-query-as-channel-p t
  "If non-nil, format text from others in a query buffer like in a channel,
otherwise format like a private message."
  :group 'erc-query
  :type 'boolean)

(defcustom erc-minibuffer-notice nil
  "If non-nil, print ERC notices for the user in the minibuffer.
Only happens when the session buffer isn't visible."
  :group 'erc-display
  :type 'boolean)

(defcustom erc-minibuffer-ignored nil
  "If non-nil, print a message in the minibuffer if we ignored something."
  :group 'erc-ignore
  :type 'boolean)

(defun erc-wash-quit-reason (reason nick login host)
  "Remove duplicate text from quit REASON.
Specifically in relation to NICK (user@host) information.  Returns REASON
unmodified if nothing can be removed.
E.g. \"Read error to Nick [user@some.host]: 110\" would be shortened to
\"Read error: 110\". The same applies for \"Ping Timeout\"."
  (setq nick (regexp-quote nick)
	login (regexp-quote login)
	host (regexp-quote host))
  (or (when (string-match (concat "^\\(Read error\\) to "
				  nick "\\[" host "\\]: "
				  "\\(.+\\)$") reason)
	(concat (match-string 1 reason) ": " (match-string 2 reason)))
      (when (string-match (concat "^\\(Ping timeout\\) for "
				  nick "\\[" host "\\]$") reason)
	(match-string 1 reason))
      reason))

(defun erc-nickname-in-use (nick reason)
  "If NICK is unavailable, tell the user the REASON.

See also `erc-display-error-notice'."
  (if (or (not erc-try-new-nick-p)
	  ;; how many default-nicks are left + one more try...
	  (eq erc-nick-change-attempt-count
	      (if (consp erc-nick)
		  (+ (length erc-nick) 1)
		1)))
      (erc-display-error-notice
       nil
       (format "Nickname %s is %s, try another." nick reason))
    (setq erc-nick-change-attempt-count (+ erc-nick-change-attempt-count 1))
    (let ((newnick (nth 1 erc-default-nicks))
	  (nicklen (cdr (assoc "NICKLEN"
			       (erc-with-server-buffer
				 erc-server-parameters)))))
      (setq erc-bad-nick t)
      ;; try to use a different nick
      (if erc-default-nicks
	  (setq erc-default-nicks (cdr erc-default-nicks)))
      (if (not newnick)
	  (setq newnick (concat (truncate-string-to-width
				 nick
				 (if (and erc-server-connected nicklen)
				     (- (string-to-number nicklen)
					(length erc-nick-uniquifier))
				   ;; rfc2812 max nick length = 9
				   ;; we must assume this is the
				   ;; server's setting if we haven't
				   ;; established a connection yet
				   (- 9 (length erc-nick-uniquifier))))
				erc-nick-uniquifier)))
      (erc-cmd-NICK newnick)
      (erc-display-error-notice
       nil
       (format "Nickname %s is %s, trying %s"
	       nick reason newnick)))))

;;; Server messages

(defgroup erc-server-hooks nil
  "Server event callbacks.
Every server event - like numeric replies - has its own hook.
Those hooks are all called using `run-hook-with-args-until-success'.
They receive as first argument the process object from where the event
originated from,
and as second argument the event parsed as a vector."
  :group 'erc-hooks)

(defun erc-display-server-message (proc parsed)
  "Display the message sent by the server as a notice."
  (erc-display-message
   parsed 'notice 'active (erc-response.contents parsed)))

(defun erc-auto-query (proc parsed)
  ;; FIXME: This needs more documentation, unless it's not a user function --
  ;; Lawrence 2004-01-08
  "Put this on `erc-server-PRIVMSG-functions'."
  (when erc-auto-query
    (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
	   (target (car (erc-response.command-args parsed)))
	   (msg (erc-response.contents parsed))
	   (query  (if (not erc-query-on-unjoined-chan-privmsg)
		       nick
		     (if (erc-current-nick-p target)
			 nick
		       target))))
      (and (not (erc-ignored-user-p (erc-response.sender parsed)))
	   (or erc-query-on-unjoined-chan-privmsg
	       (string= target (erc-current-nick)))
	   (not (erc-get-buffer query proc))
	   (not (erc-is-message-ctcp-and-not-action-p msg))
	   (let ((erc-query-display erc-auto-query))
	     (erc-cmd-QUERY query))
	   nil))))

(defun erc-is-message-ctcp-p (message)
  "Check if MESSAGE is a CTCP message or not."
  (string-match "^\C-a\\([^\C-a]*\\)\C-a?$" message))

(defun erc-is-message-ctcp-and-not-action-p (message)
  "Check if MESSAGE is a CTCP message or not."
  (and (erc-is-message-ctcp-p message)
       (not (string-match "^\C-a\\ACTION.*\C-a$" message))))

(defun erc-format-privmessage (nick msg privp msgp)
  "Format a PRIVMSG in an insertable fashion."
  (let* ((mark-s (if msgp (if privp "*" "<") "-"))
	 (mark-e (if msgp (if privp "*" ">") "-"))
	 (str	 (format "%s%s%s %s" mark-s nick mark-e msg))
	 (nick-face (if privp 'erc-nick-msg-face 'erc-nick-default-face))
	 (msg-face (if privp 'erc-direct-msg-face 'erc-default-face)))
    ;; add text properties to text before the nick, the nick and after the nick
    (erc-put-text-property 0 (length mark-s) 'face msg-face str)
    (erc-put-text-property (length mark-s) (+ (length mark-s) (length nick))
			   'face nick-face str)
    (erc-put-text-property (+ (length mark-s) (length nick)) (length str)
			   'face msg-face str)
    str))

(defcustom erc-format-nick-function 'erc-format-nick
  "*Function to format a nickname for message display."
  :group 'erc-display
  :type 'function)

(defun erc-format-nick (&optional user channel-data)
  "Return the nickname of USER.
See also `erc-format-nick-function'."
  (when user (erc-server-user-nickname user)))

(defun erc-format-@nick (&optional user channel-data)
  "Format the nickname of USER showing if USER is an operator or has voice.
Operators have \"@\" and users with voice have \"+\" as a prefix.
Use CHANNEL-DATA to determine op and voice status.
See also `erc-format-nick-function'."
  (when user
    (let ((op (and channel-data (erc-channel-user-op channel-data) "@"))
	  (voice (and channel-data (erc-channel-user-voice channel-data) "+")))
      (concat voice op (erc-server-user-nickname user)))))

(defun erc-format-my-nick ()
  "Return the beginning of this user's message, correctly propertized."
  (if erc-show-my-nick
      (let ((open "<")
	    (close "> ")
	    (nick (erc-current-nick)))
	(concat
	 (erc-propertize open 'face 'erc-default-face)
	 (erc-propertize nick 'face 'erc-my-nick-face)
	 (erc-propertize close 'face 'erc-default-face)))
    (let ((prefix "> "))
      (erc-propertize prefix 'face 'erc-default-face))))

(defun erc-echo-notice-in-default-buffer (s parsed buffer sender)
  "Echos a private notice in the default buffer, namely the
target buffer specified by BUFFER, or there is no target buffer,
the server buffer.  This function is designed to be added to
either `erc-echo-notice-hook' or `erc-echo-notice-always-hook',
and always returns t."
  (erc-display-message parsed nil buffer s)
  t)

(defun erc-echo-notice-in-target-buffer (s parsed buffer sender)
  "Echos a private notice in BUFFER, if BUFFER is non-nil.  This
function is designed to be added to either `erc-echo-notice-hook'
or `erc-echo-notice-always-hook', and returns non-nil if BUFFER
is non-nil."
  (if buffer
      (progn (erc-display-message parsed nil buffer s) t)
    nil))

(defun erc-echo-notice-in-minibuffer (s parsed buffer sender)
  "Echos a private notice in the minibuffer.  This function is
designed to be added to either `erc-echo-notice-hook' or
`erc-echo-notice-always-hook', and always returns t."
  (message "%s" (concat "NOTICE: " s))
  t)

(defun erc-echo-notice-in-server-buffer (s parsed buffer sender)
  "Echos a private notice in the server buffer.  This function is
designed to be added to either `erc-echo-notice-hook' or
`erc-echo-notice-always-hook', and always returns t."
  (erc-display-message parsed nil nil s)
  t)

(defun erc-echo-notice-in-active-non-server-buffer (s parsed buffer sender)
  "Echos a private notice in the active buffer if the active
buffer is not the server buffer.  This function is designed to be
added to either `erc-echo-notice-hook' or
`erc-echo-notice-always-hook', and returns non-nil if the active
buffer is not the server buffer."
  (if (not (eq (erc-server-buffer) (erc-active-buffer)))
      (progn (erc-display-message parsed nil 'active s) t)
    nil))

(defun erc-echo-notice-in-active-buffer (s parsed buffer sender)
  "Echos a private notice in the active buffer.  This function is
designed to be added to either `erc-echo-notice-hook' or
`erc-echo-notice-always-hook', and always returns t."
  (erc-display-message parsed nil 'active s)
  t)

(defun erc-echo-notice-in-user-buffers (s parsed buffer sender)
  "Echos a private notice in all of the buffers for which SENDER
is a member.  This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
returns non-nil if there is at least one buffer for which the
sender is a member.

See also: `erc-echo-notice-in-first-user-buffer',
`erc-buffer-list-with-nick'."
  (let ((buffers (erc-buffer-list-with-nick sender erc-server-process)))
    (if buffers
	(progn (erc-display-message parsed nil buffers s) t)
      nil)))

(defun erc-echo-notice-in-user-and-target-buffers (s parsed buffer sender)
  "Echos a private notice in BUFFER and in all of the buffers for
which SENDER is a member.  This function is designed to be added
to either `erc-echo-notice-hook' or
`erc-echo-notice-always-hook', and returns non-nil if there is
at least one buffer for which the sender is a member or the
default target.

See also: `erc-echo-notice-in-user-buffers',
`erc-buffer-list-with-nick'."
  (let ((buffers (erc-buffer-list-with-nick sender erc-server-process)))
    (add-to-list 'buffers buffer)
    (if buffers
	(progn (erc-display-message parsed nil buffers s) t)
      nil)))

(defun erc-echo-notice-in-first-user-buffer (s parsed buffer sender)
  "Echos a private notice in one of the buffers for which SENDER
is a member.  This function is designed to be added to either
`erc-echo-notice-hook' or `erc-echo-notice-always-hook', and
returns non-nil if there is at least one buffer for which the
sender is a member.

See also: `erc-echo-notice-in-user-buffers',
`erc-buffer-list-with-nick'."
  (let ((buffers (erc-buffer-list-with-nick sender erc-server-process)))
    (if buffers
	(progn (erc-display-message parsed nil (car buffers) s) t)
      nil)))

;;; Ban manipulation

(defun erc-banlist-store (proc parsed)
  "Record ban entries for a channel."
  (multiple-value-bind (channel mask whoset)
      (values-list (cdr (erc-response.command-args parsed)))
    ;; Determine to which buffer the message corresponds
    (let ((buffer (erc-get-buffer channel proc)))
      (with-current-buffer buffer
	(unless (member (cons whoset mask) erc-channel-banlist)
	  (setq erc-channel-banlist (cons (cons whoset mask)
					  erc-channel-banlist))))))
  nil)

(defun erc-banlist-finished (proc parsed)
  "Record that we have received the banlist."
  (let* ((channel (second (erc-response.command-args parsed)))
	 (buffer (erc-get-buffer channel proc)))
    (with-current-buffer buffer
      (put 'erc-channel-banlist 'received-from-server t)))
  t)					; suppress the 'end of banlist' message

(defun erc-banlist-update (proc parsed)
  "Check MODE commands for bans and update the banlist appropriately."
  ;; FIXME: Possibly incorrect. -- Lawrence 2004-05-11
  (let* ((tgt (first (erc-response.command-args parsed)))
	 (mode (erc-response.contents parsed))
	 (whoset (erc-response.sender parsed))
	 (buffer (erc-get-buffer tgt proc)))
    (when buffer
      (with-current-buffer buffer
	(cond ((not (get 'erc-channel-banlist 'received-from-server)) nil)
	      ((string-match "^\\([+-]\\)b" mode)
	       ;; This is a ban
	       (cond
		((string-match "^-" mode)
		 ;; Remove the unbanned masks from the ban list
		 (setq erc-channel-banlist
		       (erc-delete-if
			#'(lambda (y)
			    (member (upcase (cdr y))
				    (mapcar #'upcase
					    (cdr (split-string mode)))))
			erc-channel-banlist)))
		((string-match "^+" mode)
		 ;; Add the banned mask(s) to the ban list
		 (mapc
		  (lambda (mask)
		    (unless (member (cons whoset mask) erc-channel-banlist)
		      (setq erc-channel-banlist
			    (cons (cons whoset mask) erc-channel-banlist))))
		  (cdr (split-string mode))))))))))
  nil)

;; used for the banlist cmds
(defun erc-group-list (list n)
  "Group LIST into sublists of length N."
  (cond ((null list) nil)
	((null (nthcdr n list)) (list list))
	(t (cons (erc-subseq list 0 n) (erc-group-list (nthcdr n list) n)))))


;;; MOTD numreplies

(defun erc-handle-login ()
  "Handle the logging in process of connection."
  (unless erc-logged-in
    (setq erc-logged-in t)
    (message "Logging in as \'%s\'... done" (erc-current-nick))
    ;; execute a startup script
    (let ((f (erc-select-startup-file)))
      (when f
	(erc-load-script f)))))

(defun erc-connection-established (proc parsed)
  "Run just after connection.

Set user modes and run `erc-after-connect' hook."
  (with-current-buffer (process-buffer proc)
    (unless erc-server-connected ; only once per session
      (let ((server (or erc-server-announced-name
			(erc-response.sender parsed)))
	    (nick (car (erc-response.command-args parsed)))
	    (buffer (process-buffer proc)))
	(setq erc-server-connected t)
	(erc-update-mode-line)
	(erc-set-initial-user-mode nick buffer)
	(erc-server-setup-periodical-ping buffer)
	(run-hook-with-args 'erc-after-connect server nick)))))

(defun erc-set-initial-user-mode (nick buffer)
  "If `erc-user-mode' is non-nil for NICK, set the user modes.
The server buffer is given by BUFFER."
  (with-current-buffer buffer
    (when erc-user-mode
      (let ((mode (if (functionp erc-user-mode)
		      (funcall erc-user-mode)
		    erc-user-mode)))
	(when (stringp mode)
	  (erc-log (format "changing mode for %s to %s" nick mode))
	  (erc-server-send (format "MODE %s %s" nick mode)))))))

(defun erc-display-error-notice (parsed string)
  "Display STRING as an error notice.

See also `erc-display-message'."
  (erc-display-message
   parsed '(notice error) 'active string))

(defun erc-process-ctcp-query (proc parsed nick login host)
  ;; FIXME: This needs a proper docstring -- Lawrence 2004-01-08
  "Process a CTCP query."
  (let ((queries (delete "" (split-string (erc-response.contents parsed)
					  "\C-a"))))
    (if (> (length queries) 4)
	(erc-display-message
	 parsed (list 'notice 'error) proc 'ctcp-too-many)
      (if (= 0 (length queries))
	  (erc-display-message
	   parsed (list 'notice 'error) proc
	   'ctcp-empty ?n nick)
	(while queries
	  (let* ((type (upcase (car (split-string (car queries)))))
		 (hook (intern-soft (concat "erc-ctcp-query-" type "-hook"))))
	    (if (and hook (boundp hook))
		(if (string-equal type "ACTION")
		    (run-hook-with-args-until-success
		     hook proc parsed nick login host
		     (car (erc-response.command-args parsed))
		     (car queries))
		  (when erc-paranoid
		    (if (erc-current-nick-p
			 (car (erc-response.command-args parsed)))
			(erc-display-message
			 parsed 'error 'active 'ctcp-request
			 ?n nick ?u login ?h host ?r (car queries))
		      (erc-display-message
		       parsed 'error 'active 'ctcp-request-to
		       ?n nick ?u login ?h host ?r (car queries)
		       ?t (car (erc-response.command-args parsed)))))
		  (run-hook-with-args-until-success
		   hook proc nick login host
		   (car (erc-response.command-args parsed))
		   (car queries)))
	      (erc-display-message
	       parsed (list 'notice 'error) proc
	       'undefined-ctcp)))
	  (setq queries (cdr queries)))))))

(defvar erc-ctcp-query-ACTION-hook '(erc-ctcp-query-ACTION))

(defun erc-ctcp-query-ACTION (proc parsed nick login host to msg)
  "Respond to a CTCP ACTION query."
  (when (string-match "^ACTION\\s-\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg))
	  (buf (or (erc-get-buffer to proc)
		   (erc-get-buffer nick proc)
		   (process-buffer proc))))
      (erc-display-message
       parsed 'action buf
       'ACTION ?n nick ?u login ?h host ?a s))))

(defvar erc-ctcp-query-CLIENTINFO-hook '(erc-ctcp-query-CLIENTINFO))

(defun erc-ctcp-query-CLIENTINFO (proc nick login host to msg)
  "Respond to a CTCP CLIENTINFO query."
  (when (string-match "^CLIENTINFO\\(\\s-*\\|\\s-+.*\\)$" msg)
    (let ((s (erc-client-info (erc-trim-string (match-string 1 msg)))))
      (unless erc-disable-ctcp-replies
	  (erc-send-ctcp-notice nick (format "CLIENTINFO %s" s)))))
  nil)

(defvar erc-ctcp-query-ECHO-hook '(erc-ctcp-query-ECHO))
(defun erc-ctcp-query-ECHO (proc nick login host to msg)
  "Respond to a CTCP ECHO query."
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg)))
      (unless erc-disable-ctcp-replies
	(erc-send-ctcp-notice nick (format "ECHO %s" s)))))
  nil)

(defvar erc-ctcp-query-FINGER-hook '(erc-ctcp-query-FINGER))
(defun erc-ctcp-query-FINGER (proc nick login host to msg)
  "Respond to a CTCP FINGER query."
  (unless erc-disable-ctcp-replies
    (let ((s (if erc-anonymous-login
		 (format "FINGER I'm %s." (erc-current-nick))
	       (format "FINGER %s (%s@%s)."
		       (user-full-name)
		       (user-login-name)
		       (system-name))))
	  (ns (erc-time-diff erc-server-last-sent-time (erc-current-time))))
	(when (> ns 0)
	    (setq s (concat s " Idle for " (erc-sec-to-time ns))))
	(erc-send-ctcp-notice nick s)))
  nil)

(defvar erc-ctcp-query-PING-hook '(erc-ctcp-query-PING))
(defun erc-ctcp-query-PING (proc nick login host to msg)
  "Respond to a CTCP PING query."
  (when (string-match "^PING\\s-+\\(.*\\)" msg)
    (unless erc-disable-ctcp-replies
      (let ((arg (match-string 1 msg)))
	(erc-send-ctcp-notice nick (format "PING %s" arg)))))
  nil)

(defvar erc-ctcp-query-TIME-hook '(erc-ctcp-query-TIME))
(defun erc-ctcp-query-TIME (proc nick login host to msg)
  "Respond to a CTCP TIME query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice nick (format "TIME %s" (current-time-string))))
  nil)

(defvar erc-ctcp-query-USERINFO-hook '(erc-ctcp-query-USERINFO))
(defun erc-ctcp-query-USERINFO (proc nick login host to msg)
  "Respond to a CTCP USERINFO query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice nick (format "USERINFO %s" erc-user-information)))
  nil)

(defvar erc-ctcp-query-VERSION-hook '(erc-ctcp-query-VERSION))
(defun erc-ctcp-query-VERSION (proc nick login host to msg)
  "Respond to a CTCP VERSION query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice
     nick (format
	   "VERSION \C-bERC\C-b %s - an IRC client for emacs (\C-b%s\C-b)"
	   erc-version-string
	   erc-official-location)))
  nil)

(defun erc-process-ctcp-reply (proc parsed nick login host msg)
  "Process MSG as a CTCP reply."
  (let* ((type (car (split-string msg)))
	 (hook (intern (concat "erc-ctcp-reply-" type "-hook"))))
    (if (boundp hook)
	(run-hook-with-args-until-success
	 hook proc nick login host
	 (car (erc-response.command-args parsed)) msg)
      (erc-display-message
       parsed 'notice 'active
       'CTCP-UNKNOWN ?n nick ?u login ?h host ?m msg))))

(defvar erc-ctcp-reply-ECHO-hook '(erc-ctcp-reply-ECHO))
(defun erc-ctcp-reply-ECHO (proc nick login host to msg)
  "Handle a CTCP ECHO reply."
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil '(notice action) 'active
       'CTCP-ECHO ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-CLIENTINFO-hook '(erc-ctcp-reply-CLIENTINFO))
(defun erc-ctcp-reply-CLIENTINFO (proc nick login host to msg)
  "Handle a CTCP CLIENTINFO reply."
  (when (string-match "^CLIENTINFO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-CLIENTINFO ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-FINGER-hook '(erc-ctcp-reply-FINGER))
(defun erc-ctcp-reply-FINGER (proc nick login host to msg)
  "Handle a CTCP FINGER reply."
  (when (string-match "^FINGER\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-FINGER ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-PING-hook '(erc-ctcp-reply-PING))
(defun erc-ctcp-reply-PING (proc nick login host to msg)
  "Handle a CTCP PING reply."
  (if (not (string-match "^PING\\s-+\\([0-9.]+\\)" msg))
      nil
    (let ((time (match-string 1 msg)))
      (condition-case nil
	  (let ((delta (erc-time-diff (string-to-number time)
				      (erc-current-time))))
	    (erc-display-message
	     nil 'notice 'active
	     'CTCP-PING ?n nick
	     ?t (erc-sec-to-time delta)))
	(range-error
	 (erc-display-message
	  nil 'error 'active
	  'bad-ping-response ?n nick ?t time))))))

(defvar erc-ctcp-reply-TIME-hook '(erc-ctcp-reply-TIME))
(defun erc-ctcp-reply-TIME (proc nick login host to msg)
  "Handle a CTCP TIME reply."
  (when (string-match "^TIME\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-TIME ?n nick ?m message)))
  nil)

(defvar erc-ctcp-reply-VERSION-hook '(erc-ctcp-reply-VERSION))
(defun erc-ctcp-reply-VERSION (proc nick login host to msg)
  "Handle a CTCP VERSION reply."
  (when (string-match "^VERSION\\s-+\\(.*\\)\\s-*$" msg)
    (let ((message (match-string 1 msg)))
      (erc-display-message
       nil 'notice 'active
       'CTCP-VERSION ?n nick ?m message)))
  nil)

(defun erc-process-away (proc away-p)
  "Toggle the away status of the user depending on the value of AWAY-P.

If nil, set the user as away.
If non-nil, return from being away."
  (let ((sessionbuf (process-buffer proc)))
    (when sessionbuf
      (with-current-buffer sessionbuf
	(when erc-away-nickname
	  (erc-log (format "erc-process-away: away-nick: %s, away-p: %s"
			   erc-away-nickname away-p))
	  (erc-cmd-NICK (if away-p
			    erc-away-nickname
			  erc-nick)))
	(cond
	 (away-p
	  (setq erc-away (current-time)))
	 (t
	  (let ((away-time erc-away))
	    ;; away must be set to NIL BEFORE sending anything to prevent
	    ;; an infinite recursion
	    (setq erc-away nil)
	    (with-current-buffer (erc-active-buffer)
	      (when erc-public-away-p
		(erc-send-action
		 (erc-default-target)
		 (if away-time
		     (format "is back (gone for %s)"
			     (erc-sec-to-time
			      (erc-time-diff
			       (erc-emacs-time-to-erc-time away-time)
			       (erc-current-time))))
		   "is back")))))))))
    (erc-update-mode-line)))

;;;; List of channel members handling

(defun erc-channel-begin-receiving-names ()
  "Internal function.

Used when a channel names list is about to be received.  Should
be called with the current buffer set to the channel buffer.

See also `erc-channel-end-receiving-names'."
  (setq erc-channel-new-member-names (make-hash-table :test 'equal)))

(defun erc-channel-end-receiving-names ()
  "Internal function.

Used to fix `erc-channel-users' after a channel names list has been
received.  Should be called with the current buffer set to the
channel buffer.

See also `erc-channel-begin-receiving-names'."
  (maphash (lambda (nick user)
	     (if (null (gethash nick erc-channel-new-member-names))
		 (erc-remove-channel-user nick)))
	   erc-channel-users)
  (setq erc-channel-new-member-names nil))

(defun erc-parse-prefix ()
  "Return an alist of valid prefix character types and their representations.
Example: (operator) o => @, (voiced) v => +."
  (let ((str (or (cdr (assoc "PREFIX" (erc-with-server-buffer
					erc-server-parameters)))
		 ;; provide a sane default
		 "(ov)@+"))
	types chars)
    (when (string-match "^(\\([^)]+\\))\\(.+\\)$" str)
      (setq types (match-string 1 str)
	    chars (match-string 2 str))
      (let ((len (min (length types) (length chars)))
	    (i 0)
	    (alist nil))
	(while (< i len)
	  (setq alist (cons (cons (elt types i) (elt chars i))
			    alist))
	  (setq i (1+ i)))
	alist))))

(defun erc-channel-receive-names (names-string)
  "This function is for internal use only.

Update `erc-channel-users' according to NAMES-STRING.
NAMES-STRING is a string listing some of the names on the
channel."
  (let (prefix op-ch voice-ch names name op voice)
    (setq prefix (erc-parse-prefix))
    (setq op-ch (cdr (assq ?o prefix))
	  voice-ch (cdr (assq ?v prefix)))
    ;; We need to delete "" because in XEmacs, (split-string "a ")
    ;; returns ("a" "").
    (setq names (delete "" (split-string names-string)))
    (let ((erc-channel-members-changed-hook nil))
      (dolist (item names)
	(let ((updatep t)
	      ch)
	  (if (rassq (elt item 0) prefix)
	      (cond ((= (length item) 1)
		     (setq updatep nil))
		    ((eq (elt item 0) op-ch)
		     (setq name (substring item 1)
			   op 'on
			   voice 'off))
		    ((eq (elt item 0) voice-ch)
		     (setq name (substring item 1)
			   op 'off
			   voice 'on))
		    (t (setq name (substring item 1)
			     op 'off
			     voice 'off)))
	    (setq name item
		  op 'off
		  voice 'off))
	  (when updatep
	    (puthash (erc-downcase name) t
		     erc-channel-new-member-names)
	    (erc-update-current-channel-member
	     name name t op voice)))))
    (run-hooks 'erc-channel-members-changed-hook)))

(defcustom erc-channel-members-changed-hook nil
  "*This hook is called every time the variable `channel-members' changes.
The buffer where the change happened is current while this hook is called."
  :group 'erc-hooks
  :type 'hook)

(defun erc-update-user-nick (nick &optional new-nick
				  host login full-name info)
  "Updates the stored user information for the user with nickname
NICK.

See also: `erc-update-user'."
  (erc-update-user (erc-get-server-user nick) new-nick
		   host login full-name info))

(defun erc-update-user (user &optional new-nick
			     host login full-name info)
  "Update user info for USER.  USER must be an erc-server-user
struct.  Any of NEW-NICK, HOST, LOGIN, FULL-NAME, INFO which are
non-nil and not equal to the existing values for USER are used to
replace the stored values in USER.

If, and only if, a change is made,
`erc-channel-members-changed-hook' is run for each channel for
which USER is a member, and t is returned."
  (let (changed)
    (when user
      (when (and new-nick
		 (not (equal (erc-server-user-nickname user)
			     new-nick)))
	(setq changed t)
	(erc-change-user-nickname user new-nick))
      (when (and host
		 (not (equal (erc-server-user-host user) host)))
	(setq changed t)
	(setf (erc-server-user-host user) host))
      (when (and login
		 (not (equal (erc-server-user-login user) login)))
	(setq changed t)
	(setf (erc-server-user-login user) login))
      (when (and full-name
		 (not (equal (erc-server-user-full-name user)
			     full-name)))
	(setq changed t)
	(setf (erc-server-user-full-name user) full-name))
      (when (and info
		 (not (equal (erc-server-user-info user) info)))
	(setq changed t)
	(setf (erc-server-user-info user) info))
      (if changed
	  (dolist (buf (erc-server-user-buffers user))
	    (if (buffer-live-p buf)
		(with-current-buffer buf
		  (run-hooks 'erc-channel-members-changed-hook))))))
    changed))

(defun erc-update-current-channel-member
  (nick new-nick &optional add op voice host login full-name info
	update-message-time)
  "Updates the stored user information for the user with nickname
NICK.  `erc-update-user' is called to handle changes to nickname,
HOST, LOGIN, FULL-NAME, and INFO.  If OP or VOICE are non-nil,
they must be equal to either `on' or `off', in which case the
operator or voice status of the user in the current channel is
changed accordingly.  If UPDATE-MESSAGE-TIME is non-nil, the
last-message-time of the user in the current channel is set
to (current-time).

If ADD is non-nil, the user will be added with the specified
information if it is not already present in the user or channel
lists.

If, and only if, changes are made, or the user is added,
`erc-channel-members-updated-hook' is run, and t is returned.

See also: `erc-update-user' and `erc-update-channel-member'."
  (let* (changed user-changed
	 (channel-data (erc-get-channel-user nick))
	 (cuser (if channel-data (cdr channel-data)))
	 (user (if channel-data (car channel-data)
		 (erc-get-server-user nick))))
    (if cuser
	(progn
	  (erc-log (format "update-member: user = %S, cuser = %S" user cuser))
	  (when (and op
		     (not (eq (erc-channel-user-op cuser) op)))
	      (setq changed t)
	    (setf (erc-channel-user-op cuser)
		  (cond ((eq op 'on) t)
			((eq op 'off) nil)
			(t op))))
	  (when (and voice
		     (not (eq (erc-channel-user-voice cuser) voice)))
	      (setq changed t)
	    (setf (erc-channel-user-voice cuser)
		  (cond ((eq voice 'on) t)
			((eq voice 'off) nil)
			(t voice))))
	  (when update-message-time
	    (setf (erc-channel-user-last-message-time cuser) (current-time)))
	  (setq user-changed
		(erc-update-user user new-nick
				 host login full-name info)))
      (when add
	(if (null user)
	    (progn
	      (setq user (make-erc-server-user
			  :nickname nick
			  :host host
			  :full-name full-name
			  :login login
			  :info info
			  :buffers (list (current-buffer))))
	      (erc-add-server-user nick user))
	  (setf (erc-server-user-buffers user)
		(cons (current-buffer)
		      (erc-server-user-buffers user))))
	(setq cuser (make-erc-channel-user
		     :op (cond ((eq op 'on) t)
			       ((eq op 'off) nil)
			       (t op))
		     :voice (cond ((eq voice 'on) t)
				  ((eq voice 'off) nil)
				  (t voice))
		     :last-message-time
		     (if update-message-time (current-time))))
	(puthash (erc-downcase nick) (cons user cuser)
		 erc-channel-users)
	(setq changed t)))
    (when (and changed (null user-changed))
      (run-hooks 'erc-channel-members-changed-hook))
    (or changed user-changed add)))

(defun erc-update-channel-member (channel nick new-nick
				  &optional add op voice host login
				  full-name info update-message-time)
  "Updates user and channel information for the user with
nickname NICK in channel CHANNEL.

See also: `erc-update-current-channel-member'."
  (erc-with-buffer
   (channel)
   (erc-update-current-channel-member nick new-nick add op voice host
				      login full-name info
				      update-message-time)))

(defun erc-remove-current-channel-member (nick)
  "Remove NICK from current channel membership list.
Runs `erc-channel-members-changed-hook'."
  (let ((channel-data (erc-get-channel-user nick)))
    (when channel-data
      (erc-remove-channel-user nick)
      (run-hooks 'erc-channel-members-changed-hook))))

(defun erc-remove-channel-member (channel nick)
  "Remove NICK from CHANNEL's membership list.

See also `erc-remove-current-channel-member'."
  (erc-with-buffer
   (channel)
   (erc-remove-current-channel-member nick)))

(defun erc-update-channel-topic (channel topic &optional modify)
  "Find a buffer for CHANNEL and set the TOPIC for it.

If optional MODIFY is 'append or 'prepend, then append or prepend the
TOPIC string to the current topic."
  (erc-with-buffer (channel)
    (cond ((eq modify 'append)
	   (setq erc-channel-topic (concat erc-channel-topic topic)))
	  ((eq modify 'prepend)
	   (setq erc-channel-topic (concat topic erc-channel-topic)))
	  (t (setq erc-channel-topic topic)))
    (erc-update-mode-line-buffer (current-buffer))))

(defun erc-set-modes (tgt mode-string)
  "Set the modes for the TGT provided as MODE-STRING."
  (let* ((modes (erc-parse-modes mode-string))
	 (add-modes (nth 0 modes))
	 (remove-modes (nth 1 modes))
	 ;; list of triples: (mode-char 'on/'off argument)
	 (arg-modes (nth 2 modes)))
    (cond ((erc-channel-p tgt); channel modes
	   (let ((buf (and erc-server-process
			   (erc-get-buffer tgt erc-server-process))))
	     (when buf
	       (with-current-buffer buf
		 (setq erc-channel-modes add-modes)
		 (setq erc-channel-user-limit nil)
		 (setq erc-channel-key nil)
		 (while arg-modes
		   (let ((mode (nth 0 (car arg-modes)))
			 (onoff (nth 1 (car arg-modes)))
			 (arg (nth 2 (car arg-modes))))
		     (cond ((string-match "^[Ll]" mode)
			    (erc-update-channel-limit tgt onoff arg))
			   ((string-match "^[Kk]" mode)
			    (erc-update-channel-key tgt onoff arg))
			   (t nil)))
		   (setq arg-modes (cdr arg-modes)))
		 (erc-update-mode-line-buffer buf)))))
	  ;; we do not keep our nick's modes yet
	  ;;(t (setq erc-user-modes add-modes))
	  )
    ))

(defun erc-sort-strings (list-of-strings)
  "Sort LIST-OF-STRINGS in lexicographic order.

Side-effect free."
  (sort (copy-sequence list-of-strings) 'string<))

(defun erc-parse-modes (mode-string)
  "Parse MODE-STRING into a list.

Returns a list of three elements:

  (ADD-MODES REMOVE-MODES ARG-MODES).

The add-modes and remove-modes are lists of single-character strings
for modes without parameters to add and remove respectively.  The
arg-modes is a list of triples of the form:

  (MODE-CHAR ON/OFF ARGUMENT)."
  (if (string-match "^\\s-*\\(\\S-+\\)\\(\\s-.*$\\|$\\)" mode-string)
      (let ((chars (mapcar 'char-to-string (match-string 1 mode-string)))
	    ;; arguments in channel modes
	    (args-str (match-string 2 mode-string))
	    (args nil)
	    (add-modes nil)
	    (remove-modes nil)
	    (arg-modes nil); list of triples: (mode-char 'on/'off argument)
	    (add-p t))
	;; make the argument list
	(while (string-match "^\\s-*\\(\\S-+\\)\\(\\s-+.*$\\|$\\)" args-str)
	  (setq args (cons (match-string 1 args-str) args))
	  (setq args-str (match-string 2 args-str)))
	(setq args (nreverse args))
	;; collect what modes changed, and match them with arguments
	(while chars
	  (cond ((string= (car chars) "+") (setq add-p t))
		((string= (car chars) "-") (setq add-p nil))
		((string-match "^[ovbOVB]" (car chars))
		 (setq arg-modes (cons (list (car chars)
					     (if add-p 'on 'off)
					     (if args (car args) nil))
				       arg-modes))
		 (if args (setq args (cdr args))))
		((string-match "^[LlKk]" (car chars))
		 (setq arg-modes (cons (list (car chars)
					     (if add-p 'on 'off)
					     (if (and add-p args)
						 (car args) nil))
				       arg-modes))
		 (if (and add-p args) (setq args (cdr args))))
		(add-p (setq add-modes (cons (car chars) add-modes)))
		(t (setq remove-modes (cons (car chars) remove-modes))))
	  (setq chars (cdr chars)))
	(setq add-modes (nreverse add-modes))
	(setq remove-modes (nreverse remove-modes))
	(setq arg-modes (nreverse arg-modes))
	(list add-modes remove-modes arg-modes))
    nil))

(defun erc-update-modes (tgt mode-string &optional nick host login)
  "Update the mode information for TGT, provided as MODE-STRING.
Optional arguments: NICK, HOST and LOGIN - the attributes of the
person who changed the modes."
  (let* ((modes (erc-parse-modes mode-string))
	 (add-modes (nth 0 modes))
	 (remove-modes (nth 1 modes))
	 ;; list of triples: (mode-char 'on/'off argument)
	 (arg-modes (nth 2 modes)))
    ;; now parse the modes changes and do the updates
    (cond ((erc-channel-p tgt); channel modes
	   (let ((buf (and erc-server-process
			   (erc-get-buffer tgt erc-server-process))))
	     (when buf
	       ;; FIXME! This used to have an original buffer
	       ;; variable, but it never switched back to the original
	       ;; buffer. Is this wanted behavior?
	       (set-buffer buf)
	       (if (not (boundp 'erc-channel-modes))
		   (setq erc-channel-modes nil))
	       (while remove-modes
		 (setq erc-channel-modes (delete (car remove-modes)
						 erc-channel-modes)
		       remove-modes (cdr remove-modes)))
	       (while add-modes
		 (setq erc-channel-modes (cons (car add-modes)
					       erc-channel-modes)
		       add-modes (cdr add-modes)))
	       (setq erc-channel-modes (erc-sort-strings erc-channel-modes))
	       (while arg-modes
		 (let ((mode (nth 0 (car arg-modes)))
		       (onoff (nth 1 (car arg-modes)))
		       (arg (nth 2 (car arg-modes))))
		   (cond ((string-match "^[oO]" mode)
			  (erc-update-channel-member tgt arg arg nil onoff))
			 ((string-match "^[Vv]" mode)
			  (erc-update-channel-member tgt arg arg nil nil
						     onoff))
			 ((string-match "^[Ll]" mode)
			  (erc-update-channel-limit tgt onoff arg))
			 ((string-match "^[Kk]" mode)
			  (erc-update-channel-key tgt onoff arg))
			 (t nil)); only ops are tracked now
		   (setq arg-modes (cdr arg-modes))))
	       (erc-update-mode-line buf))))
	  ;; nick modes - ignored at this point
	  (t nil))))

(defun erc-update-channel-limit (channel onoff n)
  ;; FIXME: what does ONOFF actually do?  -- Lawrence 2004-01-08
  "Update CHANNEL's user limit to N."
  (if (or (not (eq onoff 'on))
	  (and (stringp n) (string-match "^[0-9]+$" n)))
      (erc-with-buffer
       (channel)
      (cond ((eq onoff 'on) (setq erc-channel-user-limit (string-to-number n)))
	    (t (setq erc-channel-user-limit nil))))))

(defun erc-update-channel-key (channel onoff key)
  "Update CHANNEL's key to KEY if ONOFF is 'on or to nil if it's 'off."
  (erc-with-buffer
   (channel)
   (cond ((eq onoff 'on) (setq erc-channel-key key))
	 (t (setq erc-channel-key nil)))))

(defun erc-handle-user-status-change (type nlh &optional l)
  "Handle changes in any user's status.

So far, only nick change is handled.

Generally, the TYPE argument is a symbol describing the change type, NLH is
a list containing the original nickname, login name and hostname for the user,
and L is a list containing additional TYPE-specific arguments.

So far the following TYPE/L pairs are supported:

       Event			TYPE		       L

    nickname change	       'nick		    (NEW-NICK)"
  (erc-log (format "user-change: type: %S  nlh: %S  l: %S" type nlh l))
  (cond
   ;; nickname change
   ((equal type 'nick)
    t)
   (t
    nil)))

(defun erc-highlight-notice (s)
  "Highlight notice message S and return it.
See also variable `erc-notice-highlight-type'."
  (cond
   ((eq erc-notice-highlight-type 'prefix)
    (erc-put-text-property 0 (length erc-notice-prefix)
			   'face 'erc-notice-face s)
    s)
   ((eq erc-notice-highlight-type 'all)
    (erc-put-text-property 0 (length s) 'face 'erc-notice-face s)
    s)
   (t s)))

(defun erc-make-notice (message)
  "Notify the user of MESSAGE."
  (when erc-minibuffer-notice
    (message "%s" message))
  (erc-highlight-notice (concat erc-notice-prefix message)))

(defun erc-highlight-error (s)
  "Highlight error message S and return it."
  (erc-put-text-property 0 (length s) 'face 'erc-error-face s)
  s)

(defun erc-put-text-property (start end property value &optional object)
  "Set text-property for an object (usually a string).
START and END define the characters covered.
PROPERTY is the text-property set, usually the symbol `face'.
VALUE is the value for the text-property, usually a face symbol such as
the face `bold' or `erc-pal-face'.
OBJECT is a string which will be modified and returned.
OBJECT is modified without being copied first.

You can redefine or `defadvice' this function in order to add
EmacsSpeak support."
  (put-text-property start end property value object))

(defun erc-list (thing)
  "Return THING if THING is a list, or a list with THING as its element."
  (if (listp thing)
      thing
    (list thing)))

(defun erc-parse-user (string)
  "Parse STRING as a user specification (nick!login@host).

Return a list of the three separate tokens."
  (cond
   ((string-match "^\\([^!\n]*\\)!\\([^@\n]*\\)@\\(.*\\)$" string)
    (list (match-string 1 string)
	  (match-string 2 string)
	  (match-string 3 string)))
   ;; Some bogus bouncers send Nick!(null), try to live with that.
   ((string-match "^\\([^!\n]*\\)!\\(.*\\)$" string)
    (list (match-string 1 string)
	  ""
	  (match-string 2 string)))
   (t
    (list string "" ""))))

(defun erc-extract-nick (string)
  "Return the nick corresponding to a user specification STRING.

See also `erc-parse-user'."
  (car (erc-parse-user string)))

(defun erc-put-text-properties (start end properties
				&optional object value-list)
  "Set text-properties for OBJECT.

START and END describe positions in OBJECT.
If VALUE-LIST is nil, set each property in PROPERTIES to t, else set
each property to the corresponding value in VALUE-LIST."
  (unless value-list
    (setq value-list (mapcar (lambda (x)
			       t)
			     properties)))
  (while (and properties value-list)
    (erc-put-text-property
     start end (pop properties) (pop value-list) object)))

;;; Input area handling:

(defun erc-beg-of-input-line ()
  "Return the value of `point' at the beginning of the input line.

Specifically, return the position of `erc-insert-marker'."
  (or (and (boundp 'erc-insert-marker)
	   (markerp erc-insert-marker))
      (error "erc-insert-marker has no value, please report a bug"))
  (marker-position erc-insert-marker))

(defun erc-end-of-input-line ()
  "Return the value of `point' at the end of the input line."
  (point-max))

(defun erc-send-current-line ()
  "Parse current line and send it to IRC."
  (interactive)
  (save-restriction
    (widen)
    (if (< (point) (erc-beg-of-input-line))
	(erc-error "Point is not in the input area")
      (let ((inhibit-read-only t)
	    (str (erc-user-input))
	    (old-buf (current-buffer)))
	(if (and (not (erc-server-buffer-live-p))
		 (not (erc-command-no-process-p str)))
	    (erc-error "ERC: No process running")
	  (erc-set-active-buffer (current-buffer))

	  ;; Kill the input and the prompt
	  (delete-region (erc-beg-of-input-line)
			 (erc-end-of-input-line))

	  (unwind-protect
	      (erc-send-input str)
	    ;; Fix the buffer if the command didn't kill it
	    (when (buffer-live-p old-buf)
	      (with-current-buffer old-buf
		(save-restriction
		  (widen)
		  (goto-char (point-max))
		  (when (processp erc-server-process)
		    (set-marker (process-mark erc-server-process) (point)))
		  (set-marker erc-insert-marker (point))
		  (let ((buffer-modified (buffer-modified-p)))
		    (erc-display-prompt)
		    (set-buffer-modified-p buffer-modified))))))

	  ;; Only when last hook has been run...
	  (run-hook-with-args 'erc-send-completed-hook str))))))

(defun erc-user-input ()
  "Return the input of the user in the current buffer."
  (buffer-substring-no-properties
   erc-input-marker
   (erc-end-of-input-line)))

(defvar erc-command-regexp "^/\\([A-Za-z']+\\)\\(\\s-+.*\\|\\s-*\\)$"
  "Regular expression used for matching commands in ERC.")

(defun erc-send-input (input)
  "Treat INPUT as typed in by the user. It is assumed that the input
and the prompt is already deleted.
This returns non-nil only if we actually send anything."
  ;; Handle different kinds of inputs
  (cond
   ;; Ignore empty input
   ((if erc-send-whitespace-lines
	(string= input "")
      (string-match "\\`[ \t\r\f\n]*\\'" input))
    (when erc-warn-about-blank-lines
      (message "Blank line - ignoring...")
      (beep))
    nil)
   (t
    (let ((str input)
	  (erc-insert-this t))
      (setq erc-send-this t)
      (run-hook-with-args 'erc-send-pre-hook input)
      (when erc-send-this
	(if (or (string-match "\n" str)
		(not (string-match erc-command-regexp str)))
	    (mapc
	     (lambda (line)
	       (mapc
		(lambda (line)
		  ;; Insert what has to be inserted for this.
		  (erc-display-msg line)
		  (erc-process-input-line (concat line "\n")
					  (null erc-flood-protect) t))
		(or (and erc-flood-protect (erc-split-line line))
		    (list line))))
	     (split-string str "\n"))
	  ;; Insert the prompt along with the command.
	  (erc-display-command str)
	  (erc-process-input-line (concat str "\n") t nil))
	t)))))

(defun erc-display-command (line)
  (when erc-insert-this
    (let ((insert-position (point)))
      (unless erc-hide-prompt
	(erc-display-prompt nil nil (erc-command-indicator)
			    (and (erc-command-indicator)
				 'erc-command-indicator-face)))
      (let ((beg (point)))
	(insert line)
	(erc-put-text-property beg (point)
			       'face 'erc-command-indicator-face)
	(insert "\n"))
      (when (processp erc-server-process)
	(set-marker (process-mark erc-server-process) (point)))
      (set-marker erc-insert-marker (point))
      (save-excursion
	(save-restriction
	  (narrow-to-region insert-position (point))
	  (run-hooks 'erc-send-modify-hook)
	  (run-hooks 'erc-send-post-hook))))))

(defun erc-display-msg (line)
  "Display LINE as a message of the user to the current target at the
current position."
  (when erc-insert-this
    (let ((insert-position (point)))
      (insert (erc-format-my-nick))
      (let ((beg (point)))
	(insert line)
	(erc-put-text-property beg (point)
			       'face 'erc-input-face))
      (insert "\n")
      (when (processp erc-server-process)
	(set-marker (process-mark erc-server-process) (point)))
      (set-marker erc-insert-marker (point))
      (save-excursion
	(save-restriction
	  (narrow-to-region insert-position (point))
	  (run-hooks 'erc-send-modify-hook)
	  (run-hooks 'erc-send-post-hook))))))

(defun erc-command-symbol (command)
  "Return the ERC command symbol for COMMAND if it exists and is bound."
  (let ((cmd (intern-soft (format "erc-cmd-%s" (upcase command)))))
    (when (fboundp cmd) cmd)))

(defun erc-extract-command-from-line (line)
  "Extract command and args from the input LINE.
If no command was given, return nil.  If command matches, return a
list of the form: (command args) where both elements are strings."
  (when (string-match erc-command-regexp line)
    (let* ((cmd (erc-command-symbol (match-string 1 line)))
	   ;; note: return is nil, we apply this simply for side effects
	   (canon-defun (while (and cmd (symbolp (symbol-function cmd)))
			  (setq cmd (symbol-function cmd))))
	   (cmd-fun (or cmd #'erc-cmd-default))
	   (arg (if cmd
		    (if (get cmd-fun 'do-not-parse-args)
			(format "%s" (match-string 2 line))
		      (delete "" (split-string (erc-trim-string
						(match-string 2 line)) " ")))
		  line)))
      (list cmd-fun arg))))

(defun erc-split-multiline-safe (string)
  "Split STRING, containing multiple lines and return them in a list.
Do it only for STRING as the complete input, do not carry unfinished
strings over to the next call."
  (let ((l ())
	(i0 0)
	(doit t))
    (while doit
      (let ((i (string-match "\r?\n" string i0))
	    (s (substring string i0)))
	(cond (i (setq l (cons (substring string i0 i) l))
		 (setq i0 (match-end 0)))
	      ((> (length s) 0)
		 (setq l (cons s l))(setq doit nil))
	      (t (setq doit nil)))))
    (nreverse l)))

;; nick handling

(defun erc-set-current-nick (nick)
  "Set the current nickname to NICK."
  (with-current-buffer (if (buffer-live-p (erc-server-buffer))
			   (erc-server-buffer)
			 (current-buffer))
    (setq erc-server-current-nick nick)))

(defun erc-current-nick ()
  "Return the current nickname."
  (with-current-buffer (if (buffer-live-p (erc-server-buffer))
			   (erc-server-buffer)
			 (current-buffer))
    erc-server-current-nick))

(defun erc-current-nick-p (nick)
  "Return non-nil if NICK is the current nickname."
  (erc-nick-equal-p nick (erc-current-nick)))

(defun erc-nick-equal-p (nick1 nick2)
  "Return non-nil if NICK1 and NICK2 are the same.

This matches strings according to the IRC protocol's case convention.

See also `erc-downcase'."
  (string= (erc-downcase nick1)
	   (erc-downcase nick2)))

;; default target handling

(defun erc-default-target ()
  "Return the current default target (as a character string) or nil if none."
  (let ((tgt (car erc-default-recipients)))
    (cond
     ((not tgt) nil)
     ((listp tgt) (cdr tgt))
     (t tgt))))

(defun erc-add-default-channel (channel)
  "Add CHANNEL to the default channel list."

  (let ((d1 (car erc-default-recipients))
	(d2 (cdr erc-default-recipients))
	(chl (downcase channel)))
      (setq erc-default-recipients
	    (cons chl erc-default-recipients))))

(defun erc-delete-default-channel (channel &optional buffer)
  "Delete CHANNEL from the default channel list."
  (let ((ob (current-buffer)))
    (with-current-buffer (if (and buffer
				  (bufferp buffer))
			     buffer
			   (current-buffer))
      (setq erc-default-recipients (delete (downcase channel)
					   erc-default-recipients)))))

(defun erc-add-query (nickname)
  "Add QUERY'd NICKNAME to the default channel list.

The previous default target of QUERY type gets removed."
  (let ((d1 (car erc-default-recipients))
	(d2 (cdr erc-default-recipients))
	(qt (cons 'QUERY (downcase nickname))))
    (if (and (listp d1)
	     (eq (car d1) 'QUERY))
	(setq erc-default-recipients (cons qt d2))
      (setq erc-default-recipients (cons qt erc-default-recipients)))))

(defun erc-delete-query ()
  "Delete the topmost target if it is a QUERY."

  (let ((d1 (car erc-default-recipients))
	(d2 (cdr erc-default-recipients)))
    (if (and (listp d1)
	     (eq (car d1) 'QUERY))
	(setq erc-default-recipients d2)
      (error "Current target is not a QUERY"))))

(defun erc-ignored-user-p (spec)
  "Return non-nil if SPEC matches something in `erc-ignore-list'.

Takes a full SPEC of a user in the form \"nick!login@host\", and
matches against all the regexp's in `erc-ignore-list'.  If any
match, returns that regexp."
  (catch 'found
    (dolist (ignored (erc-with-server-buffer erc-ignore-list))
      (if (string-match ignored spec)
	  (throw 'found ignored)))))

(defun erc-ignored-reply-p (msg tgt proc)
  ;; FIXME: this docstring needs fixing -- Lawrence 2004-01-08
  "Return non-nil if MSG matches something in `erc-ignore-reply-list'.

Takes a message MSG to a channel and returns non-nil if the addressed
user matches any regexp in `erc-ignore-reply-list'."
  (let ((target-nick (erc-message-target msg)))
    (if (not target-nick)
	nil
      (erc-with-buffer (tgt proc)
	(let ((user (erc-get-server-user target-nick)))
	  (when user
	    (erc-list-match erc-ignore-reply-list
			    (erc-user-spec user))))))))

(defun erc-message-target (msg)
  "Return the addressed target in MSG.

The addressed target is the string before the first colon in MSG."
  (if (string-match "^\\([^: \n]*\\):" msg)
      (match-string 1 msg)
    nil))

(defun erc-user-spec (user)
  "Create a nick!user@host spec from a user struct."
  (let ((nick (erc-server-user-nickname user))
	(host (erc-server-user-host user))
	(login (erc-server-user-login user)))
  (concat (if nick
	      nick
	    "")
	  "!"
	  (if login
	      login
	    "")
	  "@"
	  (if host
	      host
	    ""))))

(defun erc-list-match (lst str)
  "Return non-nil if any regexp in LST matches STR."
  (memq nil (mapcar (lambda (regexp)
		      (not (string-match regexp str)))
		    lst)))

;; other "toggles"

(defun erc-toggle-ctcp-autoresponse (&optional arg)
  "Toggle automatic CTCP replies (like VERSION and PING).

If ARG is positive, turns CTCP replies on.

If ARG is non-nil and not positive, turns CTCP replies off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-disable-ctcp-replies t))
	(arg (setq erc-disable-ctcp-replies nil))
	(t (setq erc-disable-ctcp-replies (not erc-disable-ctcp-replies))))
  (message "ERC CTCP replies are %s" (if erc-disable-ctcp-replies "OFF" "ON")))

(defun erc-toggle-flood-control (&optional arg)
  "Toggle use of flood control on sent messages.

If ARG is positive, use flood control.
If ARG is non-nil and not positive, do not use flood control.

See `erc-server-flood-margin' for an explanation of the available
flood control parameters."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-flood-protect t))
	(arg (setq erc-flood-protect nil))
	(t (setq erc-flood-protect (not erc-flood-protect))))
  (message "ERC flood control is %s"
	   (cond (erc-flood-protect "ON")
		 (t "OFF"))))

;; Some useful channel and nick commands for fast key bindings

(defun erc-invite-only-mode (&optional arg)
  "Turn on the invite only mode (+i) for the current channel.

If ARG is non-nil, turn this mode off (-i).

This command is sent even if excess flood is detected."
  (interactive "P")
  (erc-set-active-buffer (current-buffer))
  (let ((tgt (erc-default-target))
	(erc-force-send t))
    (cond ((or (not tgt) (not (erc-channel-p tgt)))
	   (erc-display-message nil 'error (current-buffer) 'no-target))
	  (arg (erc-load-irc-script-lines (list (concat "/mode " tgt " -i"))
					  t))
	  (t (erc-load-irc-script-lines (list (concat "/mode " tgt " +i"))
					t)))))

(defun erc-get-channel-mode-from-keypress (key)
  "Read a key sequence and call the corresponding channel mode function.
After doing C-c C-o, type in a channel mode letter.

C-g means quit.
RET lets you type more than one mode at a time.
If \"l\" is pressed, `erc-set-channel-limit' gets called.
If \"k\" is pressed, `erc-set-channel-key' gets called.
Anything else will be sent to `erc-toggle-channel-mode'."
  (interactive "kChannel mode (RET to set more than one): ")
  (when (featurep 'xemacs)
    (setq key (char-to-string (event-to-character (aref key 0)))))
  (cond ((equal key "\C-g")
	 (keyboard-quit))
	((equal key "\C-m")
	 (erc-insert-mode-command))
	((equal key "l")
	 (call-interactively 'erc-set-channel-limit))
	((equal key "k")
	 (call-interactively 'erc-set-channel-key))
	(t (erc-toggle-channel-mode key))))

(defun erc-toggle-channel-mode (mode &optional channel)
  "Toggle channel MODE.

If CHANNEL is non-nil, toggle MODE for that channel, otherwise use
`erc-default-target'."
  (interactive "P")
  (erc-set-active-buffer (current-buffer))
  (let ((tgt (or channel (erc-default-target)))
	(erc-force-send t))
    (cond ((or (null tgt) (null (erc-channel-p tgt)))
	   (erc-display-message nil 'error 'active 'no-target))
	  ((member mode erc-channel-modes)
	   (erc-log (format "%s: Toggle mode %s OFF" tgt mode))
	   (message "Toggle channel mode %s OFF" mode)
	   (erc-server-send (format "MODE %s -%s" tgt mode)))
	  (t (erc-log (format "%s: Toggle channel mode %s ON" tgt mode))
	     (message "Toggle channel mode %s ON" mode)
	     (erc-server-send (format "MODE %s +%s" tgt mode))))))

(defun erc-insert-mode-command ()
  "Insert the line \"/mode <current target> \" at `point'."
  (interactive)
  (let ((tgt (erc-default-target)))
    (if tgt (insert (concat "/mode " tgt " "))
      (erc-display-message nil 'error (current-buffer) 'no-target))))

(defun erc-channel-names ()
  "Run \"/names #channel\" in the current channel."
  (interactive)
  (erc-set-active-buffer (current-buffer))
  (let ((tgt (erc-default-target)))
    (if tgt (erc-load-irc-script-lines (list (concat "/names " tgt)))
      (erc-display-message nil 'error (current-buffer) 'no-target))))

(defun erc-remove-text-properties-region (start end &optional object)
  "Clears the region (START,END) in OBJECT from all colors, etc."
  (interactive "r")
  (save-excursion
    (let ((inhibit-read-only t))
      (set-text-properties start end nil object))))
(put 'erc-remove-text-properties-region 'disabled t)

;; script execution and startup

(defun erc-find-file (file &optional path)
  "Search for a FILE in the filesystem.
First the `default-directory' is searched for FILE, then any directories
specified in the list PATH.

If FILE is found, return the path to it."
  (let ((filepath file))
    (if (file-readable-p filepath) filepath
      (progn
	(while (and path
		    (progn (setq filepath (expand-file-name file (car path)))
			   (not (file-readable-p filepath))))
	  (setq path (cdr path)))
	(if path filepath nil)))))

(defun erc-select-startup-file ()
  "Select an ERC startup file.
See also `erc-startup-file-list'."
  (catch 'found
    (dolist (f erc-startup-file-list)
      (setq f (convert-standard-filename f))
      (when (file-readable-p f)
	(throw 'found f)))))

(defun erc-find-script-file (file)
  "Search for FILE in `default-directory', and any in `erc-script-path'."
  (erc-find-file file erc-script-path))

(defun erc-load-script (file)
  "Load a script from FILE.

FILE must be the full name, it is not searched in the
`erc-script-path'.  If the filename ends with `.el', then load it
as an Emacs Lisp program.  Otherwise, treat it as a regular IRC
script."
  (erc-log (concat "erc-load-script: " file))
  (cond
   ((string-match "\\.el$" file)
    (load file))
   (t
    (erc-load-irc-script file))))

(defun erc-process-script-line (line &optional args)
  "Process an IRC script LINE.

Does script-specific substitutions (script arguments, current nick,
server, etc.) in LINE and returns it.

Substitutions are: %C and %c = current target (channel or nick),
%S %s = current server, %N %n = my current nick, and %x is x verbatim,
where x is any other character;
$* = the entire argument string, $1 = the first argument, $2 = the second,
and so on."
  (if (not args) (setq args ""))
  (let* ((arg-esc-regexp "\\(\\$\\(\\*\\|[1-9][0-9]*\\)\\)\\([^0-9]\\|$\\)")
	 (percent-regexp "\\(%.\\)")
	 (esc-regexp (concat arg-esc-regexp "\\|" percent-regexp))
	 (tgt (erc-default-target))
	 (server (and (boundp 'erc-session-server) erc-session-server))
	 (nick (erc-current-nick))
	 (res "")
	 (tmp nil)
	 (arg-list nil)
	 (arg-num 0))
    (if (not tgt) (setq tgt ""))
    (if (not server) (setq server ""))
    (if (not nick) (setq nick ""))
    ;; First, compute the argument list
    (setq tmp args)
    (while (string-match "^\\s-*\\(\\S-+\\)\\(\\s-+.*$\\|$\\)" tmp)
      (setq arg-list (cons (match-string 1 tmp) arg-list))
      (setq tmp (match-string 2 tmp)))
    (setq arg-list (nreverse arg-list))
    (setq arg-num (length arg-list))
    ;; now do the substitution
    (setq tmp (string-match esc-regexp line))
    (while tmp
      ;;(message "beginning of while: tmp=%S" tmp)
      (let* ((hd (substring line 0 tmp))
	     (esc "")
	     (subst "")
	     (tail (substring line tmp)))
	(cond ((string-match (concat "^" arg-esc-regexp) tail)
	       (setq esc (match-string 1 tail))
	       (setq tail (substring tail (match-end 1))))
	      ((string-match (concat "^" percent-regexp) tail)
	       (setq esc (match-string 1 tail))
	       (setq tail (substring tail (match-end 1)))))
	;;(message "hd=%S, esc=%S, tail=%S, arg-num=%S" hd esc tail arg-num)
	(setq res (concat res hd))
	(setq subst
	      (cond ((string= esc "") "")
		    ((string-match "^\\$\\*$" esc) args)
		    ((string-match "^\\$\\([0-9]+\\)$" esc)
		     (let ((n (string-to-number (match-string 1 esc))))
		       (message "n = %S, integerp(n)=%S" n (integerp n))
		       (if (<= n arg-num) (nth (1- n) arg-list) "")))
		    ((string-match "^%[Cc]$" esc) tgt)
		    ((string-match "^%[Ss]$" esc) server)
		    ((string-match "^%[Nn]$" esc) nick)
		    ((string-match "^%\\(.\\)$" esc) (match-string 1 esc))
		    (t (erc-log (format "BUG in erc-process-script-line: bad escape sequence: %S\n" esc))
		       (message "BUG IN ERC: esc=%S" esc)
		       "")))
	(setq line tail)
	(setq tmp (string-match esc-regexp line))
	(setq res (concat res subst))
	;;(message "end of while: line=%S, res=%S, tmp=%S" line res tmp)
	))
    (setq res (concat res line))
    res))

(defun erc-load-irc-script (file &optional force)
  "Load an IRC script from FILE."
  (erc-log (concat "erc-load-script: " file))
  (let ((str (with-temp-buffer
	       (insert-file-contents file)
	       (buffer-string))))
    (erc-load-irc-script-lines (erc-split-multiline-safe str) force)))

(defun erc-load-irc-script-lines (lines &optional force noexpand)
  "Load IRC script LINES (a list of strings).

If optional NOEXPAND is non-nil, do not expand script-specific
sequences, process the lines verbatim.  Use this for multiline
user input."
  (let* ((cb (current-buffer))
	 (pnt (point))
	 (s "")
	 (sp (or (erc-command-indicator) (erc-prompt)))
	 (args (and (boundp 'erc-script-args) erc-script-args)))
    (if (and args (string-match "^ " args))
	(setq args (substring args 1)))
    ;; prepare the prompt string for echo
    (erc-put-text-property 0 (length sp)
			   'face 'erc-command-indicator-face sp)
    (while lines
      (setq s (car lines))
      (erc-log (concat "erc-load-script: CMD: " s))
      (unless (string-match "^\\s-*$" s)
	(let ((line (if noexpand s (erc-process-script-line s args))))
	  (if (and (erc-process-input-line line force)
		   erc-script-echo)
	      (progn
		(erc-put-text-property 0 (length line)
				       'face 'erc-input-face line)
		(erc-display-line (concat sp line) cb)))))
      (setq lines (cdr lines)))))

;; authentication

(defun erc-login ()
  "Perform user authentication at the IRC server."
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
		   (erc-current-nick)
		   (user-login-name)
		   (or erc-system-name (system-name))
		   erc-session-server
		   erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (format "PASS %s" erc-session-password))
    (message "Logging in without password"))
  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
	   ;; hacked - S.B.
	   (if erc-anonymous-login erc-email-userid (user-login-name))
	   "0" "*"
	   erc-session-user-full-name))
  (erc-update-mode-line))

;; connection properties' heuristics

(defun erc-determine-parameters (&optional server port nick name)
  "Determine the connection and authentication parameters.
Sets the buffer local variables:

- `erc-session-connector'
- `erc-session-server'
- `erc-session-port'
- `erc-session-full-name'
- `erc-server-current-nick'"
  (setq erc-session-connector erc-server-connect-function
        erc-session-server (erc-compute-server server)
	erc-session-port (or port erc-default-port)
	erc-session-user-full-name (erc-compute-full-name name))
  (erc-set-current-nick (erc-compute-nick nick)))

(defun erc-compute-server (&optional server)
  "Return an IRC server name.

This tries a number of increasingly more default methods until a
non-nil value is found.

- SERVER (the argument passed to this function)
- The `erc-server' option
- The value of the IRCSERVER environment variable
- The `erc-default-server' variable"
  (or server
      erc-server
      (getenv "IRCSERVER")
      erc-default-server))

(defun erc-compute-nick (&optional nick)
  "Return user's IRC nick.

This tries a number of increasingly more default methods until a
non-nil value is found.

- NICK (the argument passed to this function)
- The `erc-nick' option
- The value of the IRCNICK environment variable
- The result from the `user-login-name' function"
  (or nick
      (if (consp erc-nick) (car erc-nick) erc-nick)
      (getenv "IRCNICK")
      (user-login-name)))


(defun erc-compute-full-name (&optional full-name)
  "Return user's full name.

This tries a number of increasingly more default methods until a
non-nil value is found.

- FULL-NAME (the argument passed to this function)
- The `erc-user-full-name' option
- The value of the IRCNAME environment variable
- The result from the `user-full-name' function"
  (or full-name
      erc-user-full-name
      (getenv "IRCNAME")
      (if erc-anonymous-login "unknown" nil)
      (user-full-name)))

(defun erc-compute-port (&optional port)
  "Return a port for an IRC server.

This tries a number of increasingly more default methods until a
non-nil value is found.

- PORT (the argument passed to this function)
- The `erc-port' option
- The `erc-default-port' variable"
  (or port erc-port erc-default-port))

;; time routines

(defun erc-string-to-emacs-time (string)
  "Convert the long number represented by STRING into an Emacs format.
Returns a list of the form (HIGH LOW), compatible with Emacs time format."
  (let* ((n (string-to-number (concat string ".0"))))
    (list (truncate (/ n 65536))
	  (truncate (mod n 65536)))))

(defun erc-emacs-time-to-erc-time (time)
  "Convert Emacs TIME to a number of seconds since the epoch."
  (when time
    (+ (* (nth 0 time) 65536.0) (nth 1 time))))
;  (round (+ (* (nth 0 tm) 65536.0) (nth 1 tm))))

(defun erc-current-time ()
  "Return the `current-time' as a number of seconds since the epoch.

See also `erc-emacs-time-to-erc-time'."
  (erc-emacs-time-to-erc-time (current-time)))

(defun erc-time-diff (t1 t2)
  "Return the time difference in seconds between T1 and T2."
  (abs (- t2 t1)))

(defun erc-time-gt (t1 t2)
  "Check whether T1 > T2."
  (> t1 t2))

(defun erc-sec-to-time (ns)
  "Convert NS to a time string HH:MM.SS."
  (setq ns (truncate ns))
  (format "%02d:%02d.%02d"
	  (/ ns 3600)
	  (/ (% ns 3600) 60)
	  (% ns 60)))

(defun erc-seconds-to-string (seconds)
  "Convert a number of SECONDS into an English phrase."
  (let (days hours minutes format-args output)
    (setq days		(/ seconds 86400)
	  seconds	(% seconds 86400)
	  hours		(/ seconds 3600)
	  seconds	(% seconds 3600)
	  minutes	(/ seconds 60)
	  seconds	(% seconds 60)
	  format-args	(if (> days 0)
			    `("%d days, %d hours, %d minutes, %d seconds"
			      ,days ,hours ,minutes ,seconds)
			  (if (> hours 0)
			      `("%d hours, %d minutes, %d seconds"
				,hours ,minutes ,seconds)
			    (if (> minutes 0)
				`("%d minutes, %d seconds" ,minutes ,seconds)
			      `("%d seconds" ,seconds))))
	  output	(apply 'format format-args))
    ;; Change all "1 units" to "1 unit".
    (while (string-match "\\([^0-9]\\|^\\)1 \\S-+\\(s\\)" output)
      (setq output (erc-replace-match-subexpression-in-string
		    "" output (match-string 2 output) 2 (match-beginning 2))))
    output))


;; info

(defconst erc-clientinfo-alist
  '(("ACTION" . "is used to inform about one's current activity")
    ("CLIENTINFO" . "gives help on CTCP commands supported by client")
    ("ECHO" . "echoes its arguments back")
    ("FINGER" . "shows user's name, location, and idle time")
    ("PING" . "measures delay between peers")
    ("TIME" . "shows client-side time")
    ("USERINFO" . "shows information provided by a user")
    ("VERSION" . "shows client type and version"))
  "Alist of CTCP CLIENTINFO for ERC commands.")

(defun erc-client-info (s)
  "Return CTCP CLIENTINFO on command S.
If S is nil or an empty string then return general CLIENTINFO."
  (if (or (not s) (string= s ""))
      (concat
       (apply #'concat
	      (mapcar (lambda (e)
			(concat (car e) " "))
		      erc-clientinfo-alist))
       ": use CLIENTINFO <COMMAND> to get more specific information")
    (let ((h (assoc (upcase s) erc-clientinfo-alist)))
      (if h
	  (concat s " " (cdr h))
	(concat s ": unknown command")))))

;; Hook functions

(defun erc-directory-writable-p (dir)
  "Determine whether DIR is a writable directory.
If it doesn't exist, create it."
  (unless (file-attributes dir) (make-directory dir))
  (or (file-accessible-directory-p dir) (error "Cannot access %s" dir)))

(defun erc-kill-query-buffers (process)
  "Kill all buffers of PROCESS."
  ;; here, we only want to match the channel buffers, to avoid
  ;; "selecting killed buffers" b0rkage.
  (erc-with-all-buffers-of-server process
				  (lambda ()
				    (not (erc-server-buffer-p)))
				  (kill-buffer (current-buffer))))

(defun erc-nick-at-point ()
  "Give information about the nickname at `point'.

If called interactively, give a human readable message in the
minibuffer.  If called programmatically, return the corresponding
entry of `channel-members'."
  (interactive)
  (require 'thingatpt)
  (let* ((word (word-at-point))
	 (channel-data (erc-get-channel-user word))
	 (cuser (cdr channel-data))
	 (user (if channel-data
		   (car channel-data)
		 (erc-get-server-user word)))
	 host login full-name info nick op voice)
    (when user
      (setq nick (erc-server-user-nickname user)
	    host (erc-server-user-host user)
	    login (erc-server-user-login user)
	    full-name (erc-server-user-full-name user)
	    info (erc-server-user-info user))
      (if cuser
	  (setq op (erc-channel-user-op cuser)
		voice (erc-channel-user-voice cuser)))
      (if (interactive-p)
	  (message "%s is %s@%s%s%s"
		   nick login host
		   (if full-name (format " (%s)" full-name) "")
		   (if (or op voice)
			       (format " and is +%s%s on %s"
			       (if op "o" "")
			       (if voice "v" "")
				       (erc-default-target))
			     ""))
	user))))

(defun erc-away-time ()
  "Return non-nil if the current ERC process is set away.

In particular, the time that we were set away is returned.
See `current-time' for details on the time format."
  (erc-with-server-buffer erc-away))

;; Mode line handling

(defcustom erc-mode-line-format "%S %a"
  "A string to be formatted and shown in the mode-line in `erc-mode'.

The string is formatted using `format-spec' and the result is set as the value
of `mode-line-buffer-identification'.

The following characters are replaced:
%a: String indicating away status or \"\" if you are not away
%l: The estimated lag time to the server
%m: The modes of the channel
%n: The current nick name
%N: The name of the network
%o: The topic of the channel
%p: The session port
%t: The name of the target (channel, nickname, or servername:port)
%s: In the server-buffer, this gets filled with the value of
    `erc-server-announced-name', in a channel, the value of
    (erc-default-target) also get concatenated.
%S: In the server-buffer, this gets filled with the value of
    `erc-network', in a channel, the value of (erc-default-target)
    also get concatenated."
  :group 'erc-mode-line-and-header
  :type 'string)

(defcustom erc-header-line-format "%n on %t (%m,%l) %o"
  "A string to be formatted and shown in the header-line in `erc-mode'.
Only used starting in Emacs 21.

Set this to nil if you do not want the header line to be
displayed.

See `erc-mode-line-format' for which characters are can be used."
  :group 'erc-mode-line-and-header
  :set (lambda (sym val)
	 (set sym val)
	 (when (fboundp 'erc-update-mode-line)
	   (erc-update-mode-line nil)))
  :type '(choice (const :tag "Disabled" nil)
		 string))

(defcustom erc-header-line-uses-tabbar-p nil
  "Use tabbar mode instead of the header line to display the header."
  :group 'erc-mode-line-and-header
  :type 'boolean)

(defcustom erc-header-line-uses-help-echo-p t
  "Show the contents of the header line in the echo area or as a tooltip
when you move point into the header line."
  :group 'erc-mode-line-and-header
  :type 'boolean)

(defcustom erc-header-line-face-method nil
  "Determine what method to use when colorizing the header line text.

If nil, don't colorize the header text.
If given a function, call it and use the resulting face name.
Otherwise, use the `erc-header-line' face."
  :group 'erc-mode-line-and-header
  :type '(choice (const :tag "Don't colorize" nil)
		 (const :tag "Use the erc-header-line face" t)
		 (function :tag "Call a function")))

(defcustom erc-show-channel-key-p t
  "Show the channel key in the header line."
  :group 'erc-paranoia
  :type 'boolean)

(defcustom erc-common-server-suffixes
  '(("openprojects.net$" . "OPN")
    ("freenode.net$" . "freenode")
    ("oftc.net$" . "OFTC"))
  "Alist of common server name suffixes.
This variable is used in mode-line display to save screen
real estate.  Set it to nil if you want to avoid changing
displayed hostnames."
  :group 'erc-mode-line-and-header
  :type 'alist)

(defcustom erc-mode-line-away-status-format
  "(AWAY since %a %b %d %H:%M) "
  "When you're away on a server, this is shown in the mode line.
This should be a string with substitution variables recognized by
`format-time-string'."
  :group 'erc-mode-line-and-header
  :type 'string)

(defun erc-shorten-server-name (server-name)
  "Shorten SERVER-NAME according to `erc-common-server-suffixes'."
  (if (stringp server-name)
      (with-temp-buffer
	(insert server-name)
	(let ((alist erc-common-server-suffixes))
	  (while alist
	    (goto-char (point-min))
	(if (re-search-forward (caar alist) nil t)
	    (replace-match (cdar alist)))
	(setq alist (cdr alist))))
	(buffer-string))))

(defun erc-format-target ()
  "Return the name of the target (channel or nickname or servername:port)."
  (let ((target (erc-default-target)))
    (or target
	(concat (erc-shorten-server-name
		 (or erc-server-announced-name
		     erc-session-server))
		":" (erc-port-to-string erc-session-port)))))

(defun erc-format-target-and/or-server ()
  "Return the server name or the current target and server name combined."
  (let ((server-name (erc-shorten-server-name
		      (or erc-server-announced-name
			  erc-session-server))))
    (cond ((erc-default-target)
	   (concat (erc-string-no-properties (erc-default-target))
		   "@" server-name))
	  (server-name server-name)
	  (t (buffer-name (current-buffer))))))

(defun erc-format-network ()
  "Return the name of the network we are currently on."
  (let ((network (and (fboundp 'erc-network-name) (erc-network-name))))
    (if (and network (symbolp network))
	(symbol-name network)
      "")))

(defun erc-format-target-and/or-network ()
  "Return the network or the current target and network combined.
If the name of the network is not available, then use the
shortened server name instead."
  (let ((network-name (or (and (fboundp 'erc-network-name) (erc-network-name))
			  (erc-shorten-server-name
			   (or erc-server-announced-name
			       erc-session-server)))))
    (when (and network-name (symbolp network-name))
      (setq network-name (symbol-name network-name)))
    (cond ((erc-default-target)
	   (concat (erc-string-no-properties (erc-default-target))
		   "@" network-name))
	  (network-name network-name)
	  (t (buffer-name (current-buffer))))))

(defun erc-format-away-status ()
  "Return a formatted `erc-mode-line-away-status-format'
if `erc-away' is non-nil."
  (let ((a (erc-away-time)))
    (if a
	(format-time-string erc-mode-line-away-status-format a)
      "")))

(defun erc-format-channel-modes ()
  "Return the current channel's modes."
  (concat (apply 'concat
		 "+" erc-channel-modes)
	  (cond ((and erc-channel-user-limit erc-channel-key)
		 (if erc-show-channel-key-p
		     (format "lk %.0f %s" erc-channel-user-limit
			     erc-channel-key)
		   (format "kl %.0f" erc-channel-user-limit)))
		(erc-channel-user-limit
		 ;; Emacs has no bignums
		 (format "l %.0f" erc-channel-user-limit))
		(erc-channel-key
		 (if erc-show-channel-key-p
		     (format "k %s" erc-channel-key)
		   "k"))
		(t nil))))

(defun erc-format-lag-time ()
  "Return the estimated lag time to server, `erc-server-lag'."
  (let ((lag (erc-with-server-buffer erc-server-lag)))
    (cond (lag (format "lag:%.0f" lag))
	  (t ""))))

;; erc-goodies is required at end of this file.
(declare-function erc-controls-strip "erc-goodies" (str))

(defvar tabbar--local-hlf)

(defun erc-update-mode-line-buffer (buffer)
  "Update the mode line in a single ERC buffer BUFFER."
  (with-current-buffer buffer
    (let ((spec (format-spec-make
		 ?a (erc-format-away-status)
		 ?l (erc-format-lag-time)
		 ?m (erc-format-channel-modes)
		 ?n (or (erc-current-nick) "")
		 ?N (erc-format-network)
		 ?o (erc-controls-strip erc-channel-topic)
		 ?p (erc-port-to-string erc-session-port)
		 ?s (erc-format-target-and/or-server)
		 ?S (erc-format-target-and/or-network)
		 ?t (erc-format-target)))
	  (process-status (cond ((and (erc-server-process-alive)
				      (not erc-server-connected))
				 ":connecting")
				((erc-server-process-alive)
				 "")
				(t
				 ": CLOSED")))
	  (face (cond ((eq erc-header-line-face-method nil)
		       nil)
		      ((functionp erc-header-line-face-method)
		       (funcall erc-header-line-face-method))
		      (t
		       'erc-header-line))))
      (cond ((featurep 'xemacs)
	     (setq modeline-buffer-identification
		   (list (format-spec erc-mode-line-format spec)))
	     (setq modeline-process (list process-status)))
	    (t
	     (setq mode-line-buffer-identification
		   (list (format-spec erc-mode-line-format spec)))
	     (setq mode-line-process (list process-status))))
      (when (boundp 'header-line-format)
	(let ((header (if erc-header-line-format
			  (format-spec erc-header-line-format spec)
			nil)))
	  (cond (erc-header-line-uses-tabbar-p
		 (set (make-local-variable 'tabbar--local-hlf)
		      header-line-format)
		 (kill-local-variable 'header-line-format))
		((null header)
		 (setq header-line-format nil))
		(erc-header-line-uses-help-echo-p
		 (let ((help-echo (with-temp-buffer
				    (insert header)
				    (fill-region (point-min) (point-max))
				    (buffer-string))))
		   (setq header-line-format
			 (erc-replace-regexp-in-string
			  "%"
			  "%%"
			  (if face
			      (erc-propertize header 'help-echo help-echo
					      'face face)
			    (erc-propertize header 'help-echo help-echo))))))
		(t (setq header-line-format
			 (if face
			     (erc-propertize header 'face face)
			   header)))))))
    (if (featurep 'xemacs)
	(redraw-modeline)
      (force-mode-line-update))))

(defun erc-update-mode-line (&optional buffer)
  "Update the mode line in BUFFER.

If BUFFER is nil, update the mode line in all ERC buffers."
  (if (and buffer (bufferp buffer))
      (erc-update-mode-line-buffer buffer)
    (dolist (buf (erc-buffer-list))
      (when (buffer-live-p buf)
	(erc-update-mode-line-buffer buf)))))

;; Miscellaneous

(defun erc-port-to-string (p)
  "Convert port P to a string.
P may be an integer or a service name."
  (if (integerp p)
      (int-to-string p)
    p))

(defun erc-string-to-port (s)
  "Convert string S to either an integer port number or a service name."
  (if (numberp s)
      s
    (let ((n (string-to-number s)))
      (if (= n 0)
	  s
	n))))

(defun erc-version (&optional here)
  "Show the version number of ERC in the minibuffer.
If optional argument HERE is non-nil, insert version number at point."
  (interactive "P")
  (let ((version-string
	 (format "ERC %s (GNU Emacs %s)" erc-version-string emacs-version)))
    (if here
	(insert version-string)
      (if (interactive-p)
	  (message "%s" version-string)
	version-string))))

(defun erc-modes (&optional here)
  "Show the active ERC modes in the minibuffer.
If optional argument HERE is non-nil, insert version number at point."
  (interactive "P")
  (let ((string
	 (mapconcat 'identity
		    (let (modes (case-fold-search nil))
		      (dolist (var (apropos-internal "^erc-.*mode$"))
			(when (and (boundp var)
				   (symbol-value var))
			  (setq modes (cons (symbol-name var)
					    modes))))
		      modes)
		    ", ")))
    (if here
	(insert string)
      (if (interactive-p)
	  (message "%s" string)
	string))))

(defun erc-trim-string (s)
  "Trim leading and trailing spaces off S."
  (cond
   ((not (stringp s)) nil)
   ((string-match "^\\s-*$" s)
    "")
   ((string-match "^\\s-*\\(.*\\S-\\)\\s-*$" s)
    (match-string 1 s))
   (t
    s)))

(defun erc-arrange-session-in-multiple-windows ()
  "Open a window for every non-server buffer related to `erc-session-server'.

All windows are opened in the current frame."
  (interactive)
  (unless erc-server-process
    (error "No erc-server-process found in current buffer"))
  (let ((bufs (erc-buffer-list nil erc-server-process)))
    (when bufs
      (delete-other-windows)
      (switch-to-buffer (car bufs))
      (setq bufs (cdr bufs))
      (while bufs
	(split-window)
	(other-window 1)
	(switch-to-buffer (car bufs))
	(setq bufs (cdr bufs))
	(balance-windows)))))

(defun erc-popup-input-buffer ()
  "Provide an input buffer."
   (interactive)
   (let ((buffer-name (generate-new-buffer-name "*ERC input*"))
	 (mode (intern
		(completing-read
		 "Mode: "
		 (mapcar (lambda (e)
			   (list (symbol-name e)))
			 (apropos-internal "-mode$" 'commandp))
		 nil t))))
     (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
     (funcall mode)
     (narrow-to-region (point) (point))
     (shrink-window-if-larger-than-buffer)))

;;; Message catalog

(defun erc-make-message-variable-name (catalog entry)
  "Create a variable name corresponding to CATALOG's ENTRY."
  (intern (concat "erc-message-"
		  (symbol-name catalog) "-" (symbol-name entry))))

(defun erc-define-catalog-entry (catalog entry format-spec)
  "Set CATALOG's ENTRY to FORMAT-SPEC."
  (set (erc-make-message-variable-name catalog entry)
       format-spec))

(defun erc-define-catalog (catalog entries)
  "Define a CATALOG according to ENTRIES."
  (dolist (entry entries)
    (erc-define-catalog-entry catalog (car entry) (cdr entry))))

(erc-define-catalog
 'english
 '((bad-ping-response . "Unexpected PING response from %n (time %t)")
   (bad-syntax . "Error occurred - incorrect usage?\n%c %u\n%d")
   (incorrect-args . "Incorrect arguments. Usage:\n%c %u\n%d")
   (cannot-find-file . "Cannot find file %f")
   (cannot-read-file . "Cannot read file %f")
   (connect . "Connecting to %S:%p... ")
   (country . "%c")
   (country-unknown . "%d: No such domain")
   (ctcp-empty . "Illegal empty CTCP query received from %n. Ignoring.")
   (ctcp-request . "==> CTCP request from %n (%u@%h): %r")
   (ctcp-request-to . "==> CTCP request from %n (%u@%h) to %t: %r")
   (ctcp-too-many . "Too many CTCP queries in single message. Ignoring")
   (flood-ctcp-off . "FLOOD PROTECTION: Automatic CTCP responses turned off.")
   (flood-strict-mode
    . "FLOOD PROTECTION: Switched to Strict Flood Control mode.")
   (disconnected . "\n\nConnection failed!  Re-establishing connection...\n")
   (disconnected-noreconnect
    . "\n\nConnection failed!  Not re-establishing connection.\n")
   (finished . "\n\n*** ERC finished ***\n")
   (terminated . "\n\n*** ERC terminated: %e\n")
   (login . "Logging in as \'%n\'...")
   (nick-in-use . "%n is in use. Choose new nickname: ")
   (nick-too-long
    . "WARNING: Nick length (%i) exceeds max NICKLEN(%l) defined by server")
   (no-default-channel . "No default channel")
   (no-invitation . "You've got no invitation")
   (no-target . "No target")
   (ops . "%i operator%s: %o")
   (ops-none . "No operators in this channel.")
   (undefined-ctcp . "Undefined CTCP query received. Silently ignored")
   (variable-not-bound . "Variable not bound!")
   (ACTION . "* %n %a")
   (CTCP-CLIENTINFO . "Client info for %n: %m")
   (CTCP-ECHO . "Echo %n: %m")
   (CTCP-FINGER . "Finger info for %n: %m")
   (CTCP-PING . "Ping time to %n is %t")
   (CTCP-TIME . "Time by %n is %m")
   (CTCP-UNKNOWN . "Unknown CTCP message from %n (%u@%h): %m")
   (CTCP-VERSION . "Version for %n is %m")
   (ERROR  . "==> ERROR from %s: %c\n")
   (INVITE . "%n (%u@%h) invites you to channel %c")
   (JOIN   . "%n (%u@%h) has joined channel %c")
   (JOIN-you . "You have joined channel %c")
   (KICK . "%n (%u@%h) has kicked %k off channel %c: %r")
   (KICK-you . "You have been kicked off channel %c by %n (%u@%h): %r")
   (KICK-by-you . "You have kicked %k off channel %c: %r")
   (MODE   . "%n (%u@%h) has changed mode for %t to %m")
   (MODE-nick . "%n has changed mode for %t to %m")
   (NICK   . "%n (%u@%h) is now known as %N")
   (NICK-you . "Your new nickname is %N")
   (PART   . erc-message-english-PART)
   (PING   . "PING from server (last: %s sec. ago)")
   (PONG   . "PONG from %h (%i second%s)")
   (QUIT   . "%n (%u@%h) has quit: %r")
   (TOPIC  . "%n (%u@%h) has set the topic for %c: \"%T\"")
   (WALLOPS . "Wallops from %n: %m")
   (s004   . "%s %v %U %C")
   (s221   . "User modes for %n: %m")
   (s252   . "%i operator(s) online")
   (s253   . "%i unknown connection(s)")
   (s254   . "%i channels formed")
   (s275   . "%n %m")
   (s301   . "%n is AWAY: %r")
   (s303   . "Is online: %n")
   (s305   . "%m")
   (s306   . "%m")
   (s307   . "%n %m")
   (s311   . "%n is %f (%u@%h)")
   (s312   . "%n is/was on server %s (%c)")
   (s313   . "%n is an IRC operator")
   (s314   . "%n was %f (%u@%h)")
   (s317   . "%n has been idle for %i")
   (s317-on-since . "%n has been idle for %i, on since %t")
   (s319   . "%n is on channel(s): %c")
   (s320   . "%n is an identified user")
   (s321   . "Channel  Users  Topic")
   (s322   . "%c [%u] %t")
   (s324   . "%c modes: %m")
   (s328   . "%c URL: %u")
   (s329   . "%c was created on %t")
   (s330   . "%n %a %i")
   (s331   . "No topic is set for %c")
   (s332   . "Topic for %c: %T")
   (s333   . "%c: topic set by %n, %t")
   (s341   . "Inviting %n to channel %c")
   (s352   . "%-11c %-10n %-4a %u@%h (%f)")
   (s353   . "Users on %c: %u")
   (s367   . "Ban for %b on %c")
   (s367-set-by . "Ban for %b on %c set by %s on %t")
   (s368   . "Banlist of %c ends.")
   (s379   . "%c: Forwarded to %f")
   (s391   . "The time at %s is %t")
   (s401   . "%n: No such nick/channel")
   (s403   . "%c: No such channel")
   (s404   . "%c: Cannot send to channel")
   (s405   . "%c: You have joined too many channels")
   (s406   . "%n: There was no such nickname")
   (s412   . "No text to send")
   (s421   . "%c: Unknown command")
   (s431   . "No nickname given")
   (s432   . "%n is an erroneous nickname")
   (s442   . "%c: You're not on that channel")
   (s445   . "SUMMON has been disabled")
   (s446   . "USERS has been disabled")
   (s451   . "You have not registered")
   (s461   . "%c: not enough parameters")
   (s462   . "Unauthorized command (already registered)")
   (s463   . "Your host isn't among the privileged")
   (s464   . "Password incorrect")
   (s465   . "You are banned from this server")
   (s474   . "You can't join %c because you're banned (+b)")
   (s475   . "You must specify the correct channel key (+k) to join %c")
   (s481   . "Permission Denied - You're not an IRC operator")
   (s482   . "You need to be a channel operator of %c to do that")
   (s483   . "You can't kill a server!")
   (s484   . "Your connection is restricted!")
   (s485   . "You're not the original channel operator")
   (s491   . "No O-lines for your host")
   (s501   . "Unknown MODE flag")
   (s502   . "You can't change modes for other users")
   (s671   . "%n %a")))

(defun erc-message-english-PART (&rest args)
  "Format a proper PART message.

This function is an example on what could be done with formatting
functions."
  (let ((nick (cadr (memq ?n args)))
	(user (cadr (memq ?u args)))
	(host (cadr (memq ?h args)))
	(channel (cadr (memq ?c args)))
	(reason (cadr (memq ?r args))))
    (if (string= nick (erc-current-nick))
	(format "You have left channel %s" channel)
      (format "%s (%s@%s) has left channel %s%s"
	      nick user host channel
	      (if (not (string= reason ""))
		  (format ": %s"
			  (erc-replace-regexp-in-string "%" "%%" reason))
		"")))))


(defvar erc-current-message-catalog 'english)
(make-variable-buffer-local 'erc-current-message-catalog)

(defun erc-retrieve-catalog-entry (entry &optional catalog)
  "Retrieve ENTRY from CATALOG.

If CATALOG is nil, `erc-current-message-catalog' is used.

If ENTRY is nil in CATALOG, it is retrieved from the fallback,
english, catalog."
  (unless catalog (setq catalog erc-current-message-catalog))
  (let ((var (erc-make-message-variable-name catalog entry)))
    (if (boundp var)
	(symbol-value var)
      (when (boundp (erc-make-message-variable-name 'english entry))
	(symbol-value (erc-make-message-variable-name 'english entry))))))

(defun erc-format-message (msg &rest args)
  "Format MSG according to ARGS.

See also `format-spec'."
  (when (eq (logand (length args) 1) 1)	; oddp
    (error "Obscure usage of this function appeared"))
  (let ((entry (erc-retrieve-catalog-entry msg)))
    (when (not entry)
      (error "No format spec for message %s" msg))
    (when (functionp entry)
      (setq entry (apply entry args)))
    (format-spec entry (apply 'format-spec-make args))))

;;; Various hook functions

(add-hook 'kill-buffer-hook 'erc-kill-buffer-function)

(defcustom erc-kill-server-hook '(erc-kill-server)
  "*Invoked whenever a server-buffer is killed via `kill-buffer'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-kill-channel-hook '(erc-kill-channel)
  "*Invoked whenever a channel-buffer is killed via `kill-buffer'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-kill-buffer-hook nil
  "*Hook run whenever a non-server or channel buffer is killed.

See also `kill-buffer'."
  :group 'erc-hooks
  :type 'hook)

(defun erc-kill-buffer-function ()
  "Function to call when an ERC buffer is killed.
This function should be on `kill-buffer-hook'.
When the current buffer is in `erc-mode', this function will run
one of the following hooks:
`erc-kill-server-hook' if the server buffer was killed,
`erc-kill-channel-hook' if a channel buffer was killed,
or `erc-kill-buffer-hook' if any other buffer."
  (when (eq major-mode 'erc-mode)
    (erc-remove-channel-users)
    (cond
     ((eq (erc-server-buffer) (current-buffer))
      (run-hooks 'erc-kill-server-hook))
     ((erc-channel-p (erc-default-target))
      (run-hooks 'erc-kill-channel-hook))
     (t
      (run-hooks 'erc-kill-buffer-hook)))))

(defun erc-kill-server ()
  "Sends a QUIT command to the server when the server buffer is killed.
This function should be on `erc-kill-server-hook'."
  (when (erc-server-process-alive)
    (setq erc-server-quitting t)
    (erc-server-send (format "QUIT :%s" (funcall erc-quit-reason nil)))))

(defun erc-kill-channel ()
  "Sends a PART command to the server when the channel buffer is killed.
This function should be on `erc-kill-channel-hook'."
  (when (erc-server-process-alive)
    (let ((tgt (erc-default-target)))
      (erc-server-send (format "PART %s :%s" tgt
			       (funcall erc-part-reason nil))
		       nil tgt))))

;;; Dealing with `erc-parsed'

(defun erc-find-parsed-property ()
  "Find the next occurrence of the `erc-parsed' text property."
  (text-property-not-all (point-min) (point-max) 'erc-parsed nil))

(defun erc-restore-text-properties ()
  "Restore the property 'erc-parsed for the region."
  (let ((parsed-posn (erc-find-parsed-property)))
    (put-text-property
     (point-min) (point-max)
     'erc-parsed (when parsed-posn (erc-get-parsed-vector parsed-posn)))))

(defun erc-get-parsed-vector (point)
  "Return the whole parsed vector on POINT."
  (get-text-property point 'erc-parsed))

(defun erc-get-parsed-vector-nick (vect)
  "Return nickname in the parsed vector VECT."
  (let* ((untreated-nick (and vect (erc-response.sender vect)))
	 (maybe-nick (when untreated-nick
		       (car (split-string untreated-nick "!")))))
    (when (and (not (null maybe-nick))
	       (erc-is-valid-nick-p maybe-nick))
      untreated-nick)))

(defun erc-get-parsed-vector-type (vect)
  "Return message type in the parsed vector VECT."
  (and vect
       (erc-response.command vect)))

;; Teach url.el how to open irc:// URLs with ERC.
;; To activate, customize `url-irc-function' to `url-irc-erc'.

;;;###autoload
(defun erc-handle-irc-url (host port channel user password)
  "Use ERC to IRC on HOST:PORT in CHANNEL as USER with PASSWORD.
If ERC is already connected to HOST:PORT, simply /join CHANNEL.
Otherwise, connect to HOST:PORT as USER and /join CHANNEL."
  (let ((server-buffer
	 (car (erc-buffer-filter
	       (lambda ()
		 (and (string-equal erc-session-server host)
		      (= erc-session-port port)
		      (erc-open-server-buffer-p)))))))
    (with-current-buffer (or server-buffer (current-buffer))
      (if (and server-buffer channel)
	  (erc-cmd-JOIN channel)
	(erc-open host port (or user (erc-compute-nick)) (erc-compute-full-name)
		  (not server-buffer) password nil channel
		  (when server-buffer
		    (get-buffer-process server-buffer)))))))

(provide 'erc)

;;; Deprecated. We might eventually stop requiring the goodies automatically.
;;; IMPORTANT: This require must appear _after_ the above (provide 'erc) to
;;; avoid a recursive require error when byte-compiling the entire package.
(require 'erc-goodies)

;;; erc.el ends here
;;
;; Local Variables:
;; outline-regexp: ";;+"
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
