;;; erc-notify.el --- Online status change notification

;; Copyright (C) 2002-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@lexx.delysid.org>
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

;; This module defines a new command, /NOTIFY
;; See the docstring of `erc-cmd-NOTIFY' for details.

;;; Code:

(require 'erc)
(require 'erc-networks)
(eval-when-compile
 (require 'cl)
 (require 'pcomplete))

;;;; Customizable variables

(defgroup erc-notify nil
  "Track online status of certain nicknames."
  :group 'erc)

(defcustom erc-notify-list nil
  "*List of nicknames you want to be notified about online/offline
status change."
  :group 'erc-notify
  :type '(repeat string))

(defcustom erc-notify-interval 60
  "*Time interval (in seconds) for checking online status of notified
people."
  :group 'erc-notify
  :type 'integer)

(defcustom erc-notify-signon-hook nil
  "*Hook run after someone on `erc-notify-list' has signed on.
Two arguments are passed to the function, SERVER and NICK, both
strings."
  :group 'erc-notify
  :type 'hook
  :options '(erc-notify-signon))

(defcustom erc-notify-signoff-hook nil
  "*Hook run after someone on `erc-notify-list' has signed off.
Two arguments are passed to the function, SERVER and NICK, both
strings."
  :group 'erc-notify
  :type 'hook
  :options '(erc-notify-signoff))

(defun erc-notify-signon (server nick)
  (message "%s signed on at %s" nick server))

(defun erc-notify-signoff (server nick)
  (message "%s signed off from %s" nick server))

;;;; Internal variables

(defvar erc-last-ison nil
  "Last ISON information received through `erc-notify-timer'.")
(make-variable-buffer-local 'erc-last-ison)

(defvar erc-last-ison-time 0
  "Last time ISON was sent to the server in `erc-notify-timer'.")
(make-variable-buffer-local 'erc-last-ison-time)

;;;; Setup

(defun erc-notify-install-message-catalogs ()
  (erc-define-catalog
   'english
   '((notify_current . "Notified people online: %l")
     (notify_list    . "Current notify list: %l")
     (notify_on      . "Detected %n on IRC network %m")
     (notify_off     . "%n has left IRC network %m"))))

;;;###autoload (autoload 'erc-notify-mode "erc-notify" nil t)
(define-erc-module notify nil
  "Periodically check for the online status of certain users and report
changes."
  ((add-hook 'erc-timer-hook 'erc-notify-timer)
   (add-hook 'erc-server-JOIN-functions 'erc-notify-JOIN)
   (add-hook 'erc-server-NICK-functions 'erc-notify-NICK)
   (add-hook 'erc-server-QUIT-functions 'erc-notify-QUIT))
  ((remove-hook 'erc-timer-hook 'erc-notify-timer)
   (remove-hook 'erc-server-JOIN-functions 'erc-notify-JOIN)
   (remove-hook 'erc-server-NICK-functions 'erc-notify-NICK)
   (remove-hook 'erc-server-QUIT-functions 'erc-notify-QUIT)))

;;;; Timer handler

(defun erc-notify-timer (now)
  (when (and erc-server-connected
	     erc-notify-list
	     (> (erc-time-diff
		 erc-last-ison-time now)
		erc-notify-interval))
    (erc-once-with-server-event
     303
     '(let* ((server (erc-response.sender parsed))
	     (ison-list (delete "" (split-string
				    (erc-response.contents parsed))))
	     (new-list ison-list)
	     (old-list (erc-with-server-buffer erc-last-ison)))
	(while new-list
	  (when (not (erc-member-ignore-case (car new-list) old-list))
	    (run-hook-with-args 'erc-notify-signon-hook server (car new-list))
	    (erc-display-message
	     parsed 'notice proc
	     'notify_on ?n (car new-list) ?m (erc-network-name)))
	  (setq new-list (cdr new-list)))
	(while old-list
	  (when (not (erc-member-ignore-case (car old-list) ison-list))
	    (run-hook-with-args 'erc-notify-signoff-hook server (car old-list))
	    (erc-display-message
	     parsed 'notice proc
	     'notify_off ?n (car old-list) ?m (erc-network-name)))
	  (setq old-list (cdr old-list)))
	(setq erc-last-ison ison-list)
	t))
    (erc-server-send
     (concat "ISON " (mapconcat 'identity erc-notify-list " ")))
    (setq erc-last-ison-time now)))

(defun erc-notify-JOIN (proc parsed)
  "Check if channel joiner is on `erc-notify-list' and not on `erc-last-ison'.
If this condition is satisfied, produce a notify_on message and add the nick
to `erc-last-ison' to prevent any further notifications."
  (let ((nick (erc-extract-nick (erc-response.sender parsed))))
    (when (and (erc-member-ignore-case nick erc-notify-list)
	       (not (erc-member-ignore-case nick erc-last-ison)))
      (add-to-list 'erc-last-ison nick)
      (run-hook-with-args 'erc-notify-signon-hook
			  (or erc-server-announced-name erc-session-server)
			  nick)
      (erc-display-message
       parsed 'notice proc
       'notify_on ?n nick ?m (erc-network-name)))
    nil))

(defun erc-notify-NICK (proc parsed)
  "Check if new nick is on `erc-notify-list' and not on `erc-last-ison'.
If this condition is satisfied, produce a notify_on message and add the nick
to `erc-last-ison' to prevent any further notifications."
  (let ((nick (erc-response.contents parsed)))
    (when (and (erc-member-ignore-case nick erc-notify-list)
	       (not (erc-member-ignore-case nick erc-last-ison)))
      (add-to-list 'erc-last-ison nick)
      (run-hook-with-args 'erc-notify-signon-hook
			  (or erc-server-announced-name erc-session-server)
			  nick)
      (erc-display-message
       parsed 'notice proc
       'notify_on ?n nick ?m (erc-network-name)))
    nil))

(defun erc-notify-QUIT (proc parsed)
  "Check if quitter is on `erc-notify-list' and on `erc-last-ison'.
If this condition is satisfied, produce a notify_off message and remove the
nick from `erc-last-ison' to prevent any further notifications."
  (let ((nick (erc-extract-nick (erc-response.sender parsed))))
    (when (and (erc-member-ignore-case nick erc-notify-list)
	       (erc-member-ignore-case nick erc-last-ison))
      (setq erc-last-ison (erc-delete-if `(lambda (el)
					    (string= ,(erc-downcase nick)
						     (erc-downcase el)))
					 erc-last-ison))
      (run-hook-with-args 'erc-notify-signoff-hook
			  (or erc-server-announced-name erc-session-server)
			  nick)
      (erc-display-message
       parsed 'notice proc
       'notify_off ?n nick ?m (erc-network-name)))
    nil))

;;;; User level command

;;;###autoload
(defun erc-cmd-NOTIFY (&rest args)
  "Change `erc-notify-list' or list current notify-list members online.
Without args, list the current list of notified people online,
with args, toggle notify status of people."
  (cond
   ((null args)
    ;; Print current notified people (online)
    (let ((ison (erc-with-server-buffer erc-last-ison)))
      (if (not ison)
	  (erc-display-message
	   nil 'notice 'active "No ison-list yet!")
	(erc-display-message
	 nil 'notice 'active
	 'notify_current ?l ison))))
   ((string= (car args) "-l")
    (erc-display-message nil 'notice 'active
			 'notify_list ?l (mapconcat 'identity erc-notify-list
						    " ")))
   (t
    (while args
      (if (erc-member-ignore-case (car args) erc-notify-list)
	  (progn
	    (setq erc-notify-list (delete (car args) erc-notify-list))
	    ;; Remove the nick from the value of erc-last-ison in
	    ;; every server buffer.  This prevents seeing a signoff
	    ;; notification for a nick that you have just _removed_
	    ;; from your notify list.
	    (dolist (buf (erc-buffer-list))
	      (with-current-buffer buf
		(if (erc-server-buffer-p)
		    (setq erc-last-ison (delete (car args) erc-last-ison))))))
	(setq erc-notify-list (cons (erc-string-no-properties (car args))
				    erc-notify-list)))
      (setq args (cdr args)))
    (erc-display-message
     nil 'notice 'active
     'notify_list ?l (mapconcat 'identity erc-notify-list " "))))
  t)

(autoload 'pcomplete-erc-all-nicks "erc-pcomplete")

;;;###autoload
(defun pcomplete/erc-mode/NOTIFY ()
  (pcomplete-here (pcomplete-erc-all-nicks)))

(erc-notify-install-message-catalogs)

(provide 'erc-notify)

;;; erc-notify.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
