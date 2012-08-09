;;; org-irc.el --- Store links to IRC sessions
;;
;; Copyright (C) 2008-2012 Free Software Foundation, Inc.
;;
;; Author: Philip Jackson <emacs@shellarchive.co.uk>
;; Keywords: erc, irc, link, org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements links to an IRC session from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.
;;
;; Please customize the variable `org-modules' to select
;; extensions you would like to use, and to deselect those which you don't
;; want.
;;
;; Please note that at the moment only ERC is supported.  Other clients
;; shouldn't be difficult to add though.
;;
;; Then set `org-irc-link-to-logs' to non-nil if you would like a
;; file:/ type link to be created to the current line in the logs or
;; to t if you would like to create an irc:/ style link.
;;
;; Links within an org buffer might look like this:
;;
;; [[irc:/irc.freenode.net/#emacs/bob][chat with bob in #emacs on freenode]]
;; [[irc:/irc.freenode.net/#emacs][#emacs on freenode]]
;; [[irc:/irc.freenode.net/]]
;;
;; If, when the resulting link is visited, there is no connection to a
;; requested server then one will be created.

;;; Code:

(require 'org)

;; Declare the function form ERC that we use.
(declare-function erc-current-logfile "erc-log" (&optional buffer))
(declare-function erc-prompt "erc" ())
(declare-function erc-default-target "erc" ())
(declare-function erc-channel-p "erc" (channel))
(declare-function erc-buffer-filter "erc" (predicate &optional proc))
(declare-function erc-server-buffer "erc" ())
(declare-function erc-get-server-nickname-list "erc" ())
(declare-function erc-cmd-JOIN "erc" (channel &optional key))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))

(defvar org-irc-client 'erc
  "The IRC client to act on.")
(defvar org-irc-link-to-logs nil
  "Non-nil will store a link to the logs, nil will store an irc: style link.")

(defvar erc-default-port)   ; dynamically scoped from erc.el
(defvar erc-session-port)   ; dynamically scoped form erc-backend.el
(defvar erc-session-server) ; dynamically scoped form erc-backend.el

;; Generic functions/config (extend these for other clients)

(add-to-list 'org-store-link-functions 'org-irc-store-link)

(org-add-link-type "irc" 'org-irc-visit nil)

(defun org-irc-visit (link)
  "Parse LINK and dispatch to the correct function based on the client found."
  (let ((link (org-irc-parse-link link)))
    (cond
      ((eq org-irc-client 'erc)
       (org-irc-visit-erc link))
      (t
       (error "erc only known client")))))

(defun org-irc-parse-link (link)
  "Parse an IRC LINK and return the attributes found.
Parse a LINK that looks like server:port/chan/user (port, chan
and user being optional) and return any of the port, channel or user
attributes that are found."
  (let* ((parts (split-string link "/" t))
	 (len (length parts)))
    (when (or (< len 1) (> len 3))
      (error "Failed to parse link needed 1-3 parts, got %d" len))
    (setcar parts (split-string (car parts) ":" t))
    parts))

;;;###autoload
(defun org-irc-store-link ()
  "Dispatch to the appropriate function to store a link to an IRC session."
  (cond
    ((eq major-mode 'erc-mode)
     (org-irc-erc-store-link))))

(defun org-irc-elipsify-description (string &optional after)
  "Remove unnecessary white space from STRING and add ellipses if necessary.
Strip starting and ending white space from STRING and replace any
chars that the value AFTER with '...'"
  (let* ((after (number-to-string (or after 30)))
	 (replace-map (list (cons "^[ \t]*" "")
			    (cons "[ \t]*$" "")
			    (cons (concat "^\\(.\\{" after
					  "\\}\\).*") "\\1..."))))
    (mapc (lambda (x)
	    (when (string-match (car x) string)
	      (setq string (replace-match (cdr x) nil nil string))))
	  replace-map)
    string))

;; ERC specific functions

(defun org-irc-erc-get-line-from-log (erc-line)
  "Find the best line to link to from the ERC logs given ERC-LINE as a start.
If the user is on the ERC-prompt then search backward for the
first non-blank line, otherwise return the current line.  The
result is a cons of the filename and search string."
  (erc-save-buffer-in-logs)
  (require 'erc-log)
  (with-current-buffer (find-file-noselect (erc-current-logfile))
    (goto-char (point-max))
    (list
     (abbreviate-file-name buffer-file-name)
     ;; can we get a '::' part?
     (if (string= erc-line (erc-prompt))
	 (progn
	   (goto-char (point-at-bol))
	   (when (search-backward-regexp "^[^	]" nil t)
	     (buffer-substring-no-properties (point-at-bol)
					     (point-at-eol))))
	 (when (search-backward erc-line nil t)
	   (buffer-substring-no-properties (point-at-bol)
					   (point-at-eol)))))))

(defun org-irc-erc-store-link ()
  "Store a link to the IRC log file or the session itself.
Depending on the variable `org-irc-link-to-logs' store either a
link to the log file for the current session or an irc: link to
the session itself."
  (require 'erc-log)
  (if org-irc-link-to-logs
      (let* ((erc-line (buffer-substring-no-properties
			(point-at-bol) (point-at-eol)))
	     (parsed-line (org-irc-erc-get-line-from-log erc-line)))
	(if (erc-logging-enabled nil)
	    (progn
	      (org-store-link-props
	       :type "file"
	       :description (concat "'" (org-irc-elipsify-description
					 (cadr parsed-line) 20)
				    "' from an IRC conversation")
	       :link (concat "file:" (car parsed-line) "::"
			     (cadr parsed-line)))
	      t)
	    (error "This ERC session is not being logged")))
      (let* ((link-text (org-irc-get-erc-link))
	     (link (org-irc-parse-link link-text)))
	(if link-text
	    (progn
	      (org-store-link-props
	       :type "irc"
	       :link (org-make-link "irc:/" link-text)
	       :description (concat "irc session '" link-text "'")
	       :server (car (car link))
	       :port (or (string-to-number (cadr (pop link))) erc-default-port)
	       :nick (pop link))
	      t)
	    (error "Failed to create ('irc:/' style) ERC link")))))

(defun org-irc-get-erc-link ()
  "Return an org compatible irc:/ link from an ERC buffer."
  (let* ((session-port (if (numberp erc-session-port)
			   (number-to-string erc-session-port)
			   erc-session-port))
	  (link (concat erc-session-server ":" session-port)))
    (concat link "/"
	    (if (and (erc-default-target)
		     (erc-channel-p (erc-default-target))
		     (car (get-text-property (point) 'erc-data)))
		;; we can get a nick
		(let ((nick (car (get-text-property (point) 'erc-data))))
		  (concat (erc-default-target) "/" nick))
		(erc-default-target)))))

(defun org-irc-get-current-erc-port ()
  "Return the current port as a number.
Return the current port number or, if none is set, return the ERC
default."
  (cond
    ((stringp erc-session-port)
     (string-to-number erc-session-port))
    ((numberp erc-session-port)
     erc-session-port)
    (t
     erc-default-port)))

(defun org-irc-visit-erc (link)
  "Visit an ERC buffer based on criteria found in LINK."
  (require 'erc)
  (require 'erc-log)
  (let* ((server (car (car link)))
	 (port (or (string-to-number (cadr (pop link))) erc-default-port))
	 (server-buffer)
	 (buffer-list
	  (erc-buffer-filter
	   (lambda nil
	     (let ((tmp-server-buf (erc-server-buffer)))
	       (and tmp-server-buf
		    (with-current-buffer tmp-server-buf
		      (and
		       (eq (org-irc-get-current-erc-port) port)
		       (string= erc-session-server server)
		       (setq server-buffer tmp-server-buf)))))))))
    (if buffer-list
	(let ((chan-name (pop link)))
	  ;; if we got a channel name then switch to it or join it
	  (if chan-name
	      (let ((chan-buf (catch 'found
				(dolist (x buffer-list)
				  (if (string= (buffer-name x) chan-name)
				      (throw 'found x))))))
		(if chan-buf
		    (progn
		      (org-pop-to-buffer-same-window chan-buf)
		      ;; if we got a nick, and they're in the chan,
		      ;; then start a chat with them
		      (let ((nick (pop link)))
			(when nick
			  (if (member nick (erc-get-server-nickname-list))
			      (progn
				(goto-char (point-max))
				(insert (concat nick ": ")))
			      (error "%s not found in %s" nick chan-name)))))
		    (progn
		      (org-pop-to-buffer-same-window server-buffer)
		      (erc-cmd-JOIN chan-name))))
	      (org-pop-to-buffer-same-window server-buffer)))
	;; no server match, make new connection
	(erc-select :server server :port port))))

(provide 'org-irc)

;;; org-irc.el ends here
