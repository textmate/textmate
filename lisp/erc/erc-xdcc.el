;;; erc-xdcc.el --- XDCC file-server support for ERC

;; Copyright (C) 2003-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm, processes

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

;; This file provides a very simple XDCC file server for ERC.

;;; Code:

(require 'erc-dcc)

(defcustom erc-xdcc-files nil
  "*List of files to offer via XDCC.
Your friends should issue \"/ctcp yournick XDCC list\" to see this."
  :group 'erc-dcc
  :type '(repeat file))

(defcustom erc-xdcc-verbose-flag t
  "*Report XDCC CTCP requests in the server buffer."
  :group 'erc-dcc
  :type 'boolean)

(defcustom erc-xdcc-handler-alist
  '(("help" . erc-xdcc-help)
    ("list" . erc-xdcc-list)
    ("send" . erc-xdcc-send))
  "*Sub-command handler alist for XDCC CTCP queries."
  :group 'erc-dcc
  :type '(alist :key-type (string :tag "Sub-command") :value-type function))

(defcustom erc-xdcc-help-text
  '(("Hey " nick ", wondering how this works?  Pretty easy.")
    ("Available commands: XDCC ["
     (mapconcat 'car erc-xdcc-handler-alist "|") "]")
    ("Type \"/ctcp " (erc-current-nick)
     " XDCC list\" to see the list of offered files, then type \"/ctcp "
     (erc-current-nick) " XDCC send #\" to get a particular file number."))
  "*Help text sent in response to XDCC help command.
A list of messages, each consisting of strings and expressions, expressions
being evaluated and should return strings."
  :group 'erc-dcc
  :type '(repeat (repeat :tag "Message" (choice string sexp))))

;;;###autoload (autoload 'erc-xdcc-mode "erc-xdcc")
(define-erc-module xdcc nil
  "Act as an XDCC file-server."
  nil nil)

;;;###autoload
(defun erc-xdcc-add-file (file)
  "Add a file to `erc-xdcc-files'."
  (interactive "fFilename to add to XDCC: ")
  (if (file-exists-p file)
      (add-to-list 'erc-xdcc-files file)))

(defun erc-xdcc-reply (proc nick msg)
  (process-send-string proc
   (format "PRIVMSG %s :%s\n" nick msg)))

;; CTCP query handlers

(defvar erc-ctcp-query-XDCC-hook '(erc-xdcc)
  "Hook called whenever a CTCP XDCC message is received.")

(defun erc-xdcc (proc nick login host to query)
  "Handle incoming CTCP XDCC queries."
  (when erc-xdcc-verbose-flag
    (erc-display-message nil 'notice proc
     (format "XDCC %s (%s@%s) sends %S" nick login host query)))
  (let* ((args (cdr (delete "" (split-string query " "))))
	 (handler (cdr (assoc (downcase (car args)) erc-xdcc-handler-alist))))
    (if (and handler (functionp handler))
	(funcall handler proc nick login host (cdr args))
      (erc-xdcc-reply
       proc nick
       (format "Unknown XDCC sub-command, try \"/ctcp %s XDCC help\""
	       (erc-current-nick))))))

(defun erc-xdcc-help (proc nick login host args)
  "Send basic help information to NICK."
  (mapc
   (lambda (msg)
     (erc-xdcc-reply proc nick
      (mapconcat (lambda (elt) (if (stringp elt) elt (eval elt))) msg "")))
   erc-xdcc-help-text))

(defun erc-xdcc-list (proc nick login host args)
  "Show the contents of `erc-xdcc-files' via privmsg to NICK."
  (if (null erc-xdcc-files)
      (erc-xdcc-reply proc nick "No files offered, sorry")
    (erc-xdcc-reply proc nick "Num  Filename")
    (erc-xdcc-reply proc nick "---  -------------")
    (let ((n 0))
      (dolist (file erc-xdcc-files)
	(erc-xdcc-reply proc nick
	 (format "%02d.  %s"
		 (setq n (1+ n))
		 (erc-dcc-file-to-name file)))))))

(defun erc-xdcc-send (proc nick login host args)
  "Send a file to NICK."
  (let ((n (string-to-number (car args)))
	(len (length erc-xdcc-files)))
    (cond
     ((= len 0)
      (erc-xdcc-reply proc nick "No files offered, sorry"))
     ((or (< n 1) (> n len))
      (erc-xdcc-reply proc nick (format "%d out of range" n)))
     (t (erc-dcc-send-file nick (nth (1- n) erc-xdcc-files) proc)))))

(provide 'erc-xdcc)

;;; erc-xdcc.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

