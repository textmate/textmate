;;; erc-join.el --- autojoin channels on connect and reconnects

;; Copyright (C) 2002-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Keywords: irc
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcAutoJoin

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

;; This allows us to customize an `erc-autojoin-channels-alist'.  As
;; we /JOIN and /PART channels, this alist is updated to reflect our
;; current setup, so that when we reconnect, we rejoin the same
;; channels.  The alist can be customized, so that the customized
;; value will be used when we reconnect in our next Emacs session.

;;; Code:

(require 'erc)
(eval-when-compile (require 'cl))

(defgroup erc-autojoin nil
  "Enable autojoining."
  :group 'erc)

;;;###autoload (autoload 'erc-autojoin-mode "erc-join" nil t)
(define-erc-module autojoin nil
  "Makes ERC autojoin on connects and reconnects."
  ((add-hook 'erc-after-connect 'erc-autojoin-channels)
   (add-hook 'erc-nickserv-identified-hook 'erc-autojoin-after-ident)
   (add-hook 'erc-server-JOIN-functions 'erc-autojoin-add)
   (add-hook 'erc-server-PART-functions 'erc-autojoin-remove))
  ((remove-hook 'erc-after-connect 'erc-autojoin-channels)
   (remove-hook 'erc-nickserv-identified-hook 'erc-autojoin-after-ident)
   (remove-hook 'erc-server-JOIN-functions 'erc-autojoin-add)
   (remove-hook 'erc-server-PART-functions 'erc-autojoin-remove)))

(defcustom erc-autojoin-channels-alist nil
  "Alist of channels to autojoin on IRC networks.
Every element in the alist has the form (SERVER . CHANNELS).
SERVER is a regexp matching the server, and channels is the
list of channels to join.

Customize this variable to set the value for your first connect.
Once you are connected and join and part channels, this alist
keeps track of what channels you are on, and will join them
again when you get disconnected.  When you restart Emacs, however,
those changes are lost, and the customization you saved the last
time is used again."
  :group 'erc-autojoin
  :type '(repeat (cons :tag "Server"
		       (regexp :tag "Name")
		       (repeat :tag "Channels"
			       (string :tag "Name")))))

(defcustom erc-autojoin-timing 'connect
  "When ERC should attempt to autojoin a channel.
If the value is `connect', autojoin immediately on connecting.
If the value is `ident', autojoin after successful NickServ
identification, or after `erc-autojoin-delay' seconds.
Any other value means the same as `connect'."
  :group 'erc-autojoin
  :version "24.1"
  :type  '(choice (const :tag "On Connection" 'connect)
		  (const :tag "When Identified" 'ident)))

(defcustom erc-autojoin-delay 30
  "Number of seconds to wait before attempting to autojoin channels.
This only takes effect if `erc-autojoin-timing' is `ident'.
If NickServ identification occurs before this delay expires, ERC
autojoins immediately at that time."
  :group 'erc-autojoin
  :version "24.1"
  :type  'integer)

(defcustom erc-autojoin-domain-only t
  "Truncate host name to the domain name when joining a server.
If non-nil, and a channel on the server a.b.c is joined, then
only b.c is used as the server for `erc-autojoin-channels-alist'.
This is important for networks that redirect you to other
servers, presumably in the same domain."
  :group 'erc-autojoin
  :type 'boolean)

(defvar erc--autojoin-timer nil)
(make-variable-buffer-local 'erc--autojoin-timer)

(defun erc-autojoin-channels-delayed (server nick buffer)
  "Attempt to autojoin channels.
This is called from a timer set up by `erc-autojoin-channels'."
  (if erc--autojoin-timer
      (setq erc--autojoin-timer
	    (erc-cancel-timer erc--autojoin-timer)))
  (with-current-buffer buffer
    ;; Don't kick of another delayed autojoin or try to wait for
    ;; another ident response:
    (let ((erc-autojoin-delay -1)
	  (erc-autojoin-timing 'connect))
      (erc-log "Delayed autojoin started (no ident success detected yet)")
      (erc-autojoin-channels server nick))))

(defun erc-autojoin-after-ident (network nick)
  "Autojoin channels in `erc-autojoin-channels-alist'.
This function is run from `erc-nickserv-identified-hook'."
  (if erc--autojoin-timer
      (setq erc--autojoin-timer
	    (erc-cancel-timer erc--autojoin-timer)))
  (when (eq erc-autojoin-timing 'ident)
    (let ((server (or erc-server-announced-name erc-session-server))
	  (joined (mapcar (lambda (buf)
			    (with-current-buffer buf (erc-default-target)))
			  (erc-channel-list erc-server-process))))
      ;; We may already be in these channels, e.g. because the
      ;; autojoin timer went off.
      (dolist (l erc-autojoin-channels-alist)
	(when (string-match (car l) server)
	  (dolist (chan (cdr l))
	    (unless (erc-member-ignore-case chan joined)
	      (erc-server-send (concat "join " chan))))))))
  nil)

(defun erc-autojoin-channels (server nick)
  "Autojoin channels in `erc-autojoin-channels-alist'."
  (if (eq erc-autojoin-timing 'ident)
      ;; Prepare the delayed autojoin timer, in case ident doesn't
      ;; happen within the allotted time limit:
      (when (> erc-autojoin-delay 0)
	(setq erc--autojoin-timer
	      (run-with-timer erc-autojoin-delay nil
			      'erc-autojoin-channels-delayed
			      server nick (current-buffer))))
    ;; `erc-autojoin-timing' is `connect':
    (dolist (l erc-autojoin-channels-alist)
      (when (string-match (car l) server)
	(dolist (chan (cdr l))
	  (erc-server-send (concat "join " chan))))))
  ;; Return nil to avoid stomping on any other hook funcs.
  nil)

(defun erc-autojoin-add (proc parsed)
  "Add the channel being joined to `erc-autojoin-channels-alist'."
  (let* ((chnl (erc-response.contents parsed))
	 (nick (car (erc-parse-user (erc-response.sender parsed))))
	 (server (with-current-buffer (process-buffer proc)
		   (or erc-server-announced-name erc-session-server))))
    (when (erc-current-nick-p nick)
      (when (and erc-autojoin-domain-only
		 (string-match "[^.\n]+\\.\\([^.\n]+\\.[^.\n]+\\)$" server))
	(setq server (match-string 1 server)))
      (let ((elem (assoc server erc-autojoin-channels-alist)))
	(if elem
	    (unless (member chnl (cdr elem))
	      (setcdr elem (cons chnl (cdr elem))))
	  (setq erc-autojoin-channels-alist
		(cons (list server chnl)
		      erc-autojoin-channels-alist))))))
  ;; We must return nil to tell ERC to continue running the other
  ;; functions.
  nil)

;; (erc-parse-user "kensanata!~user@dclient217-162-233-228.hispeed.ch")

(defun erc-autojoin-remove (proc parsed)
  "Remove the channel being left from `erc-autojoin-channels-alist'."
  (let* ((chnl (car (erc-response.command-args parsed)))
	 (nick (car (erc-parse-user (erc-response.sender parsed))))
	 (server (with-current-buffer (process-buffer proc)
		   (or erc-server-announced-name erc-session-server))))
    (when (erc-current-nick-p nick)
      (when (and erc-autojoin-domain-only
		 (string-match "[^.\n]+\\.\\([^.\n]+\\.[^.\n]+\\)$" server))
	(setq server (match-string 1 server)))
      (let ((elem (assoc server erc-autojoin-channels-alist)))
	(when elem
	  (setcdr elem (delete chnl (cdr elem)))
	  (unless (cdr elem)
	    (setq erc-autojoin-channels-alist
		  (delete elem erc-autojoin-channels-alist)))))))
  ;; We must return nil to tell ERC to continue running the other
  ;; functions.
  nil)

(provide 'erc-join)

;;; erc-join.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

