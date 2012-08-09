;;; erc-ezbounce.el ---  Handle EZBounce bouncer commands

;; Copyright (C) 2002, 2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
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

;;; Code:

(require 'erc)
(eval-when-compile (require 'cl))

(defgroup erc-ezbounce nil
  "Interface to the EZBounce IRC bouncer (a virtual IRC server)"
  :group 'erc)

(defcustom erc-ezb-regexp "^ezbounce!srv$"
  "Regexp used by the EZBouncer to identify itself to the user."
  :group 'erc-ezbounce
  :type 'string)

(defcustom erc-ezb-login-alist '()
  "Alist of logins suitable for the server we're connecting to.

The alist's format is as follows:

 (((server . port) . (username . password))
  ((server . port) . (username . password))
  ...)"
  :group 'erc-ezbounce
  :type '(repeat
	  (cons (cons :tag "Server"
		     string
		     string)
		(cons :tag "Login"
		      string
		      string))))

(defvar erc-ezb-action-alist '(("^\\[awaiting login/pass command\\]$" . erc-ezb-identify)
			       ("^\\[use /quote CONN <server> to connect\\]$"    . erc-ezb-select)
			       ("^ID +IRC NICK +TO +TIME$" . erc-ezb-init-session-list)
			       ("^$"                       . erc-ezb-end-of-session-list)
			       (".*"                       . erc-ezb-add-session))
  "Alist of actions to take on NOTICEs from EZBounce.")


(defvar erc-ezb-session-list '()
  "List of detached EZBounce sessions.")
(make-variable-buffer-local 'erc-ezb-session-list)

(defvar erc-ezb-inside-session-listing nil
  "Indicate whether current notices are expected to be EZB session listings.")

;;;###autoload
(defun erc-cmd-ezb (line &optional force)
  "Send EZB commands to the EZBouncer verbatim."
  (erc-server-send (concat "EZB " line)))
(put 'erc-cmd-EZB 'do-not-parse-args t)

;;;###autoload
(defun erc-ezb-get-login (server port)
  "Return an appropriate EZBounce login for SERVER and PORT.
Look up entries in `erc-ezb-login-alist'. If the username or password
in the alist is `nil', prompt for the appropriate values."
  (let ((login (cdr (assoc (cons server port) erc-ezb-login-alist))))
    (when login
      (let ((username (car login))
	    (password (cdr login)))
	(when (null username)
	  (setq username (read-from-minibuffer (format "EZBounce user name for %s:%s: " server port))))
	(when (null password)
	  (setq password (read-passwd (format "EZBounce password for %s:%s: " server port))))
	(cons username password)))))

;;;###autoload
(defun erc-ezb-lookup-action (message)
  (let ((function-alist erc-ezb-action-alist)
	found)
    (while (and (not found)
		function-alist)
      (let ((regexp (caar function-alist))
	    (function (cdar function-alist)))
	(when (string-match regexp message)
	  (setq found function))
	(setq function-alist (cdr function-alist))))
    found))

;;;###autoload
(defun erc-ezb-notice-autodetect (proc parsed)
  "React on an EZBounce NOTICE request."
  (let* ((sender (erc-response.sender parsed))
	 (message (erc-response.contents parsed))
	 (function (erc-ezb-lookup-action message)))
    (when (and (string-match erc-ezb-regexp sender)
	       function)
      (funcall function message)))
  nil)

;;;###autoload
(defun erc-ezb-identify (message)
  "Identify to the EZBouncer server."
  (let ((login (erc-ezb-get-login erc-session-server (erc-port-to-string erc-session-port))))
    (unless (null login)
      (let ((username (car login))
	    (pass (cdr login)))
	(erc-server-send (concat "LOGIN " username " " pass))))))

;;;###autoload
(defun erc-ezb-init-session-list (message)
  "Reset the EZBounce session list to nil."
  (setq erc-ezb-session-list nil)
  (setq erc-ezb-inside-session-listing t))

;;;###autoload
(defun erc-ezb-end-of-session-list (message)
  "Indicate the end of the EZBounce session listing."
  (setq erc-ezb-inside-session-listing nil))

;;;###autoload
(defun erc-ezb-add-session (message)
  "Add an EZBounce session to the session list."
  (when (and erc-ezb-inside-session-listing
	     (string-match "^\\([^ \n]+\\) +\\([^ \n]+\\) +\\([^ \n]+\\) +\\([^ \n]+\\)$" message))
    (let ((id (match-string 1 message))
	  (nick (match-string 2 message))
	  (to   (match-string 3 message)))
      (add-to-list 'erc-ezb-session-list (list id nick to)))))

;;;###autoload
(defun erc-ezb-select (message)
  "Select an IRC server to use by EZBounce, in ERC style."
  (unless (and erc-ezb-session-list
	       (erc-ezb-select-session))
    (let* ((server (read-from-minibuffer
		    "IRC server: " "" nil nil 'erc-server-history-list))
	   (port
	    (erc-string-to-port
	     (read-from-minibuffer "IRC port: "
				   (erc-port-to-string "6667")))))
      (erc-server-send (format "CONN %s %s" server port)))))


;;;###autoload
(defun erc-ezb-select-session ()
  "Select a detached EZBounce session."
  (let ((session (completing-read "Existing Session (RET to enter a new one): "
			       erc-ezb-session-list)))
    (if (string= session "")
	nil
      (erc-server-send (format "REATTACH %s" session)))))


;;;###autoload
(defun erc-ezb-initialize ()
  "Add EZBouncer convenience functions to ERC."
  (add-hook 'erc-server-NOTICE-functions 'erc-ezb-notice-autodetect))

(provide 'erc-ezbounce)

;;; erc-ezbounce.el ends here
