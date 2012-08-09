;;; erc-autoaway.el --- Provides autoaway for ERC

;; Copyright (C) 2002-2004, 2006-2012  Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcAutoAway

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

;; TODO:
;; - Legacy names: erc-auto-discard-away, erc-auto-set-away

;;; Code:

(require 'erc)

(defgroup erc-autoaway nil
  "Set yourself automatically away after some idletime and set
yourself back when you type something."
  :group 'erc)

(defvar erc-autoaway-idletimer nil
  "The Emacs idletimer.
This is only used when `erc-autoaway-idle-method' is set to 'emacs.")

(defvar erc-autoaway-last-sent-time (erc-current-time)
  "The last time the user sent something.")

(defvar erc-autoaway-caused-away nil
  "Indicates whether this module was responsible for setting the
user's away status.")

(defvar erc-autoaway-idle-seconds)

(defun erc-autoaway-reestablish-idletimer ()
  "Reestablish the Emacs idletimer.
If `erc-autoaway-idle-method' is 'emacs, you must call this
function each time you change `erc-autoaway-idle-seconds'."
  (interactive)
  (when erc-autoaway-idletimer
    (erc-cancel-timer erc-autoaway-idletimer))
  (setq erc-autoaway-idletimer
	(run-with-idle-timer erc-autoaway-idle-seconds
			     t
			     'erc-autoaway-set-away
			     erc-autoaway-idle-seconds)))

(defun erc-autoaway-some-server-buffer ()
  "Return some ERC server buffer if its connection is alive.
If none is found, return nil."
  (car (erc-buffer-list #'erc-open-server-buffer-p)))

(defun erc-autoaway-insinuate-maybe (&optional server &rest ignored)
  "Add autoaway reset function to `post-command-hook' if at least one
ERC process is alive.

This is used when `erc-autoaway-idle-method' is 'user."
  (when (or server (erc-autoaway-some-server-buffer))
    (add-hook 'post-command-hook 'erc-autoaway-reset-idle-user)))

(defun erc-autoaway-remove-maybe (&rest ignored)
  "Remove the autoaway reset function from `post-command-hook' if
no ERC process is alive.

This is used when `erc-autoaway-idle-method' is 'user."
  (unless (erc-autoaway-some-server-buffer)
    (remove-hook 'post-command-hook 'erc-autoaway-reset-idle-user)))

;;;###autoload (autoload 'erc-autoaway-mode "erc-autoaway")
(define-erc-module autoaway nil
  "In ERC autoaway mode, you can be set away automatically.
If `erc-auto-set-away' is set, then you will be set away after
the number of seconds specified in `erc-autoaway-idle-seconds'.

There are several kinds of being idle:

User idle time measures how long you have not been sending any
commands to Emacs.  This is the default.

Emacs idle time measures how long Emacs has been idle.  This is
currently not useful, since Emacs is non-idle when it handles
ping-pong with IRC servers.  See `erc-autoaway-idle-method'
for more information.

IRC idle time measures how long since you last sent something (see
`erc-autoaway-last-sent-time').

If `erc-auto-discard-away' is set, then typing anything, will
set you no longer away.

Related variables: `erc-public-away-p' and `erc-away-nickname'."
  ;; Enable:
  ((when (boundp 'erc-autoaway-idle-method)
     (add-hook 'erc-connect-pre-hook 'erc-autoaway-reset-indicators)
     (setq erc-autoaway-last-sent-time (erc-current-time))
     (cond
      ((eq erc-autoaway-idle-method 'irc)
       (add-hook 'erc-send-completed-hook 'erc-autoaway-reset-idle-irc)
       (add-hook 'erc-server-001-functions 'erc-autoaway-reset-idle-irc))
      ((eq erc-autoaway-idle-method 'user)
       (add-hook 'erc-after-connect 'erc-autoaway-insinuate-maybe)
       (add-hook 'erc-disconnected-hook 'erc-autoaway-remove-maybe)
       (erc-autoaway-insinuate-maybe))
      ((eq erc-autoaway-idle-method 'emacs)
       (erc-autoaway-reestablish-idletimer)))
     (add-hook 'erc-timer-hook 'erc-autoaway-possibly-set-away)
     (add-hook 'erc-server-305-functions 'erc-autoaway-reset-indicators)))
  ;; Disable:
  ((when (boundp 'erc-autoaway-idle-method)
     (remove-hook 'erc-connect-pre-hook 'erc-autoaway-reset-indicators)
     (cond
      ((eq erc-autoaway-idle-method 'irc)
       (remove-hook 'erc-send-completed-hook 'erc-autoaway-reset-idle-irc)
       (remove-hook 'erc-server-001-functions 'erc-autoaway-reset-idle-irc))
      ((eq erc-autoaway-idle-method 'user)
       (remove-hook 'post-command-hook 'erc-autoaway-reset-idle-user)
       (remove-hook 'erc-after-connect 'erc-autoaway-insinuate-maybe)
       (remove-hook 'erc-disconnected-hook 'erc-autoaway-remove-maybe))
      ((eq erc-autoaway-idle-method 'emacs)
       (erc-cancel-timer erc-autoaway-idletimer)
       (setq erc-autoaway-idletimer nil)))
     (remove-hook 'erc-timer-hook 'erc-autoaway-possibly-set-away)
     (remove-hook 'erc-server-305-functions 'erc-autoaway-reset-indicators))))

(defcustom erc-autoaway-idle-method 'user
  "*The method used to determine how long you have been idle.
If 'user, the time of the last command sent to Emacs is used.
If 'emacs, the idle time in Emacs is used.
If 'irc, the time of the last IRC command is used.

The time itself is specified by `erc-autoaway-idle-seconds'.

See `erc-autoaway-mode' for more information on the various
definitions of being idle."
  :group 'erc-autoaway
  :type '(choice (const :tag "User idle time" user)
		 (const :tag "Emacs idle time" emacs)
		 (const :tag "Last IRC action" irc))
  :set (lambda (sym val)
	 (if erc-autoaway-mode
	     (progn
	       (erc-autoaway-disable)
	       (set sym val)
	       (erc-autoaway-enable))
	   (set sym val))))

(defcustom erc-auto-set-away t
  "*If non-nil, set away after `erc-autoaway-idle-seconds' seconds of idling.
ERC autoaway mode can set you away when you idle, and set you no
longer away when you type something.  This variable controls whether
you will be set away when you idle.  See `erc-auto-discard-away' for
the other half."
  :group 'erc-autoaway
  :type 'boolean)

(defcustom erc-auto-discard-away t
  "*If non-nil, sending anything when away automatically discards away state.
ERC autoaway mode can set you away when you idle, and set you no
longer away when you type something.  This variable controls whether
you will be set no longer away when you type something.  See
`erc-auto-set-away' for the other half.
See also `erc-autoaway-no-auto-discard-regexp'."
  :group 'erc-autoaway
  :type 'boolean)

(defcustom erc-autoaway-no-auto-discard-regexp "^/g?away.*$"
  "*Input that matches this will not automatically discard away status.
See `erc-auto-discard-away'."
  :group 'erc-autoaway
  :type 'regexp)

(defcustom erc-autoaway-idle-seconds 1800
  "*Number of seconds after which ERC will set you automatically away.
If you are changing this variable using lisp instead of customizing it,
you have to run `erc-autoaway-reestablish-idletimer' afterwards."
  :group 'erc-autoaway
  :set (lambda (sym val)
	 (set-default sym val)
	 (when (eq erc-autoaway-idle-method 'emacs)
	   (erc-autoaway-reestablish-idletimer)))
  :type 'number)

(defcustom erc-autoaway-message
  "I'm gone (autoaway after %i seconds of idletime)"
  "*Message ERC will use when setting you automatically away.
It is used as a `format' string with the argument of the idletime
in seconds."
  :group 'erc-autoaway
  :type 'string)

(defun erc-autoaway-reset-idle-user (&rest stuff)
  "Reset the stored user idle time.
This is one global variable since a user talking on one net can
talk on another net too."
  (when erc-auto-discard-away
    (erc-autoaway-set-back #'erc-autoaway-remove-maybe))
  (setq erc-autoaway-last-sent-time (erc-current-time)))

(defun erc-autoaway-reset-idle-irc (line &rest stuff)
  "Reset the stored IRC idle time.
This is one global variable since a user talking on one net can
talk on another net too."
  (when (and erc-auto-discard-away
	     (stringp line)
	     (not (string-match erc-autoaway-no-auto-discard-regexp line)))
    (erc-autoaway-set-back))
  (setq erc-autoaway-last-sent-time (erc-current-time)))

(defun erc-autoaway-set-back (&optional none-alive-func)
  "Discard the away state globally.

NONE-ALIVE-FUNC is the function to call if no ERC processes are alive."
  (let ((server-buffer (erc-autoaway-some-server-buffer)))
    (if (and erc-autoaway-caused-away
	     (buffer-live-p server-buffer)
	     (with-current-buffer server-buffer erc-away))
	(erc-cmd-GAWAY "")
      (when none-alive-func (funcall none-alive-func)))))

(defun erc-autoaway-some-open-server-buffer ()
  "Return some ERC server buffer if its connection is alive and the
user is not away.
If none is found, return nil."
  (car (erc-buffer-list (lambda ()
			  (and (erc-open-server-buffer-p)
			       (not erc-away))))))

(defun erc-autoaway-possibly-set-away (current-time)
  "Set autoaway when `erc-auto-set-away' is true and the idletime is
exceeds `erc-autoaway-idle-seconds'."
  ;; A test for (erc-server-process-alive) is not necessary, because
  ;; this function is called from `erc-timer-hook', which is called
  ;; whenever the server sends something to the client.
  (when (and erc-server-connected
	     erc-auto-set-away
	     (not erc-autoaway-caused-away)
	     (erc-autoaway-some-open-server-buffer))
    (let ((idle-time (erc-time-diff erc-autoaway-last-sent-time
				    current-time)))
      (when (>= idle-time erc-autoaway-idle-seconds)
	(erc-display-message
	 nil 'notice nil
	 (format "Setting automatically away after %i seconds of idle-time"
		 idle-time))
	(erc-autoaway-set-away idle-time t)))))

(defun erc-autoaway-set-away (idle-time &optional notest)
  "Set the away state globally.

If NOTEST is specified, do not check to see whether there is an
active server buffer available."
  ;; Note that the idle timer runs, even when Emacs is inactive.  In
  ;; order to prevent flooding when we connect, we test for an
  ;; existing process.
  (when (or notest (erc-autoaway-some-open-server-buffer))
    (setq erc-autoaway-caused-away t)
    (erc-cmd-GAWAY (format erc-autoaway-message idle-time))))

(defun erc-autoaway-reset-indicators (&rest stuff)
  "Reset indicators used by the erc-autoaway module."
  (setq erc-autoaway-last-sent-time (erc-current-time))
  (setq erc-autoaway-caused-away nil))

(provide 'erc-autoaway)

;;; erc-autoaway.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
