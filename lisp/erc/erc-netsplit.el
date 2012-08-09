;;; erc-netsplit.el --- Reduce JOIN/QUIT messages on netsplits

;; Copyright (C) 2002-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
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

;; This module hides quit/join messages if a netsplit occurs.
;; To enable, add the following to your ~/.emacs:
;; (require 'erc-netsplit)
;; (erc-netsplit-mode 1)

;;; Code:

(require 'erc)
(eval-when-compile (require 'cl))

(defgroup erc-netsplit nil
  "Netsplit detection tries to automatically figure when a
netsplit happens, and filters the QUIT messages. It also keeps
track of netsplits, so that it can filter the JOIN messages on a netjoin too."
  :group 'erc)

;;;###autoload (autoload 'erc-netsplit-mode "erc-netsplit")
(define-erc-module netsplit nil
  "This mode hides quit/join messages if a netsplit occurs."
  ((erc-netsplit-install-message-catalogs)
   (add-hook 'erc-server-JOIN-functions 'erc-netsplit-JOIN)
   (add-hook 'erc-server-MODE-functions 'erc-netsplit-MODE)
   (add-hook 'erc-server-QUIT-functions 'erc-netsplit-QUIT)
   (add-hook 'erc-timer-hook 'erc-netsplit-timer))
  ((remove-hook 'erc-server-JOIN-functions 'erc-netsplit-JOIN)
   (remove-hook 'erc-server-MODE-functions 'erc-netsplit-MODE)
   (remove-hook 'erc-server-QUIT-functions 'erc-netsplit-QUIT)
   (remove-hook 'erc-timer-hook 'erc-netsplit-timer)))

(defcustom erc-netsplit-show-server-mode-changes-flag nil
  "Set to t to enable display of server mode changes."
  :group 'erc-netsplit
  :type 'boolean)

(defcustom erc-netsplit-debug nil
  "If non-nil, debug messages will be shown in the
sever buffer."
  :group 'erc-netsplit
  :type 'boolean)

(defcustom erc-netsplit-regexp
  "^[^ @!\"\n]+\\.[^ @!\n]+ [^ @!\n]+\\.[^ @!\"\n]+$"
  "This regular expression should match quit reasons produced
by netsplits."
  :group 'erc-netsplit
  :type 'regexp)

(defcustom erc-netsplit-hook nil
  "Run whenever a netsplit is detected the first time.
Args: PROC is the process the netsplit originated from and
      SPLIT is the netsplit (e.g. \"server.name.1 server.name.2\")."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-netjoin-hook nil
  "Run whenever a netjoin is detected the first time.
Args: PROC is the process the netjoin originated from and
      SPLIT is the netsplit (e.g. \"server.name.1 server.name.2\")."
  :group 'erc-hooks
  :type 'hook)

(defvar erc-netsplit-list nil
  "This is a list of the form
\((\"a.b.c.d e.f.g\" TIMESTAMP FIRST-JOIN \"nick1\" ... \"nickn\") ...)
where FIRST-JOIN is t or nil, depending on whether or not the first
join from that split has been detected or not.")
(make-variable-buffer-local 'erc-netsplit-list)

(defun erc-netsplit-install-message-catalogs ()
  (erc-define-catalog
   'english
   '((netsplit	       . "netsplit: %s")
     (netjoin	       . "netjoin: %s, %N were split")
     (netjoin-done     . "netjoin: All lost souls are back!")
     (netsplit-none    . "No netsplits in progress")
     (netsplit-wholeft . "split: %s missing: %n %t"))))

(defun erc-netsplit-JOIN (proc parsed)
  "Show/don't show rejoins."
  (let ((nick (erc-response.sender parsed))
	(no-next-hook nil))
    (dolist (elt erc-netsplit-list)
      (if (member nick (nthcdr 3 elt))
	  (progn
	    (if (not (caddr elt))
		(progn
		  (erc-display-message
		   parsed 'notice (process-buffer proc)
		   'netjoin ?s (car elt) ?N (length (nthcdr 3 elt)))
		  (setcar (nthcdr 2 elt) t)
		  (run-hook-with-args 'erc-netjoin-hook proc (car elt))))
	    ;; need to remove this nick, perhaps the whole entry here.
	    ;; Note that by removing the nick now, we can't tell if further
	    ;; join messages (for other channels) should also be
	    ;; suppressed.
	    (if (null (nthcdr 4 elt))
		(progn
		  (erc-display-message
		   parsed 'notice (process-buffer proc)
		   'netjoin-done ?s (car elt))
		  (setq erc-netsplit-list (delq elt erc-netsplit-list)))
	      (delete nick elt))
	    (setq no-next-hook t))))
    no-next-hook))

(defun erc-netsplit-MODE (proc parsed)
  "Hide mode changes from servers."
  ;; regexp matches things with a . in them, and no ! or @ in them.
  (when (string-match "^[^@!\n]+\\.[^@!\n]+$" (erc-response.sender parsed))
    (and erc-netsplit-debug
	 (erc-display-message
	  parsed 'notice (process-buffer proc)
	  "[debug] server mode change."))
    (not erc-netsplit-show-server-mode-changes-flag)))

(defun erc-netsplit-QUIT (proc parsed)
  "Detect netsplits."
  (let ((split (erc-response.contents parsed))
	(nick (erc-response.sender parsed))
	ass)
    (when (string-match erc-netsplit-regexp split)
      (setq ass (assoc split erc-netsplit-list))
      (if ass
	  ;; element for this netsplit exists already
	  (progn
	    (setcdr (nthcdr 2 ass) (cons nick (nthcdr 3 ass)))
	    (when (caddr ass) 
	      ;; There was already a netjoin for this netsplit, it
	      ;; seems like the old one didn't get finished...
	      (erc-display-message 
	       parsed 'notice (process-buffer proc)
	       'netsplit ?s split)
	      (setcar (nthcdr 2 ass) t)
	      (run-hook-with-args 'erc-netsplit-hook proc split)))
	;; element for this netsplit does not yet exist
	(setq erc-netsplit-list
	      (cons (list split
			  (erc-current-time)
			  nil
			  nick)
		    erc-netsplit-list))
	(erc-display-message
	 parsed 'notice (process-buffer proc)
	 'netsplit ?s split)
	(run-hook-with-args 'erc-netsplit-hook proc split))
      t)))

(defun erc-netsplit-timer (now)
  "Clean cruft from `erc-netsplit-list' older than 10 minutes."
  (when erc-server-connected
    (dolist (elt erc-netsplit-list)
      (when (> (erc-time-diff (cadr elt) now) 600)
	(when erc-netsplit-debug
	  (erc-display-message
	   nil 'notice (current-buffer)
	   (concat "Netsplit: Removing " (car elt))))
	(setq erc-netsplit-list (delq elt erc-netsplit-list))))))

;;;###autoload
(defun erc-cmd-WHOLEFT ()
  "Show who's gone."
  (erc-with-server-buffer
    (if (null erc-netsplit-list)
	(erc-display-message
	 nil 'notice 'active
	 'netsplit-none)
      (dolist (elt erc-netsplit-list)
	(erc-display-message
	 nil 'notice 'active
	 'netsplit-wholeft ?s (car elt)
	 ?n (mapconcat 'erc-extract-nick (nthcdr 3 elt) " ")
	 ?t (if (caddr elt)
		"(joining)"
	      "")))))
  t)

(defalias 'erc-cmd-WL 'erc-cmd-WHOLEFT)

(provide 'erc-netsplit)

;;; erc-netsplit.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

