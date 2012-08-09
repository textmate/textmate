;;; erc-speedbar.el --- Speedbar support for ERC

;; Copyright (C) 2001-2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Contributor: Eric M. Ludlam <eric@siege-engine.com>

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

;; This module provides integration of ERC into the Speedbar.

;;; TODO / ideas:

;; * Write intelligent update function:
;;   update-channel, update-nick, remove-nick-from-channel, ...
;; * Use indicator-strings for op/voice
;; * Extract/convert face notes field from bbdb if available and show
;;   it using sb-image.el
;;
;;; Code:

(require 'erc)
(require 'speedbar)
(condition-case nil (require 'dframe) (error nil))
(eval-when-compile (require 'cl))

;;; Customization:

(defgroup erc-speedbar nil
  "Integration of ERC in the Speedbar"
  :group 'erc)

(defcustom erc-speedbar-sort-users-type 'activity
  "How channel nicknames are sorted.

'activity     - Sort users by channel activity
'alphabetical - Sort users alphabetically
nil           - Do not sort users"
  :group 'erc-speedbar
  :type '(choice (const :tag "Sort users by channel activity" activity)
		 (const :tag "Sort users alphabetically" alphabetical)
		 (const :tag "Do not sort users" nil)))

(defvar erc-speedbar-key-map nil
  "Keymap used when in erc display mode.")

(defun erc-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance ERC."
  (if erc-speedbar-key-map
      nil
    (setq erc-speedbar-key-map (speedbar-make-specialized-keymap))

    ;; Basic tree features
    (define-key erc-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key erc-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key erc-speedbar-key-map "+" 'speedbar-expand-line)
    (define-key erc-speedbar-key-map "=" 'speedbar-expand-line)
    (define-key erc-speedbar-key-map "-" 'speedbar-contract-line))

  (speedbar-add-expansion-list '("ERC" erc-speedbar-menu-items
				 erc-speedbar-key-map
				 erc-speedbar-server-buttons))
  (speedbar-add-mode-functions-list
   '("ERC" (speedbar-item-info . erc-speedbar-item-info))))

(defvar erc-speedbar-menu-items
  '(["Goto buffer" speedbar-edit-line t]
    ["Expand Node" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Node" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))])
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (erc-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'erc-install-speedbar-variables))

;;; ERC hierarchy display method
;;;###autoload
(defun erc-speedbar-browser ()
  "Initialize speedbar to display an ERC browser.
This will add a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into Info mode on speedbar.
  (speedbar-change-initial-expansion-list "ERC")
  (speedbar-get-focus))

(defun erc-speedbar-buttons (buffer)
  "Create buttons for speedbar in BUFFER."
  (erase-buffer)
  (let (serverp chanp queryp)
    (with-current-buffer buffer
      (setq serverp (erc-server-buffer-p))
      (setq chanp (erc-channel-p (erc-default-target)))
      (setq queryp (erc-query-buffer-p)))
    (cond (serverp
	   (erc-speedbar-channel-buttons nil 0 buffer))
	  (chanp
	   (erc-speedbar-insert-target buffer 0)
	   (forward-line -1)
	   (erc-speedbar-expand-channel "+" buffer 0))
	  (queryp
	   (erc-speedbar-insert-target buffer 0))
	  (t (ignore)))))

(defun erc-speedbar-server-buttons (directory depth)
  "Insert the initial list of servers you are connected to."
  (let ((servers (erc-buffer-list
		  (lambda ()
		    (eq (current-buffer)
			(process-buffer erc-server-process))))))
    (when servers
      (speedbar-with-writable
	(dolist (server servers)
	  (speedbar-make-tag-line
	   'bracket ?+ 'erc-speedbar-expand-server server
	   (buffer-name server) 'erc-speedbar-goto-buffer server nil
	   depth))
	t))))

(defun erc-speedbar-expand-server (text server indent)
  (cond ((string-match "+" text)
	 (speedbar-change-expand-button-char ?-)
	 (if (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (erc-speedbar-channel-buttons nil (1+ indent) server)))
	     (speedbar-change-expand-button-char ?-)
	   (speedbar-change-expand-button-char ??)))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun erc-speedbar-channel-buttons (directory depth server-buffer)
  (when (get-buffer server-buffer)
    (let* ((proc (with-current-buffer server-buffer erc-server-process))
	   (targets (erc-buffer-list
		     (lambda ()
		       (not (eq (process-buffer erc-server-process)
				(current-buffer))))
		     proc)))
      (when targets
	(speedbar-with-writable
	  (dolist (target targets)
	    (erc-speedbar-insert-target target depth))
	  t)))))

(defun erc-speedbar-insert-target (buffer depth)
  (if (with-current-buffer buffer
	(erc-channel-p (erc-default-target)))
      (speedbar-make-tag-line
       'bracket ?+ 'erc-speedbar-expand-channel buffer
       (buffer-name buffer) 'erc-speedbar-goto-buffer buffer nil
       depth)
    ;; Query target
    (speedbar-make-tag-line
     nil nil nil nil
     (buffer-name buffer) 'erc-speedbar-goto-buffer buffer nil
     depth)))

(defun erc-speedbar-expand-channel (text channel indent)
  "For the line matching TEXT, in CHANNEL, expand or contract a line.
INDENT is the current indentation level."
  (cond
   ((string-match "+" text)
    (speedbar-change-expand-button-char ?-)
    (speedbar-with-writable
     (save-excursion
       (end-of-line) (forward-char 1)
       (let ((modes (with-current-buffer channel
		      (concat (apply 'concat
				     erc-channel-modes)
			      (cond
			       ((and erc-channel-user-limit
				     erc-channel-key)
				(if erc-show-channel-key-p
				    (format "lk %.0f %s"
					    erc-channel-user-limit
					    erc-channel-key)
				  (format "kl %.0f" erc-channel-user-limit)))
			       (erc-channel-user-limit
				;; Emacs has no bignums
				(format "l %.0f" erc-channel-user-limit))
			       (erc-channel-key
				(if erc-show-channel-key-p
				    (format "k %s" erc-channel-key)
				  "k"))
			       (t "")))))
	     (topic (erc-controls-interpret
		     (with-current-buffer channel erc-channel-topic))))
	 (speedbar-make-tag-line
	  'angle ?i nil nil
	  (concat "Modes: +" modes) nil nil nil
	  (1+ indent))
	 (unless (string= topic "")
	   (speedbar-make-tag-line
	    'angle ?i nil nil
	    (concat "Topic: " topic) nil nil nil
	    (1+ indent)))
	 (let ((names (cond ((eq erc-speedbar-sort-users-type 'alphabetical)
			     (erc-sort-channel-users-alphabetically
			      (with-current-buffer channel
				(erc-get-channel-user-list))))
			    ((eq erc-speedbar-sort-users-type 'activity)
			     (erc-sort-channel-users-by-activity
			      (with-current-buffer channel
				(erc-get-channel-user-list))))
			    (t (with-current-buffer channel
				 (erc-get-channel-user-list))))))
	   (when names
	     (speedbar-with-writable
	      (dolist (entry names)
		(erc-speedbar-insert-user entry ?+ (1+ indent))))))))))
   ((string-match "-" text)
    (speedbar-change-expand-button-char ?+)
    (speedbar-delete-subblock indent))
   (t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun erc-speedbar-insert-user (entry exp-char indent)
  "Insert one user based on the channel member list ENTRY.
EXP-CHAR is the expansion character to use.
INDENT is the current indentation level."
  (let* ((user (car entry))
	 (cuser (cdr entry))
	 (nick (erc-server-user-nickname user))
	 (host (erc-server-user-host user))
	 (info (erc-server-user-info user))
	 (login (erc-server-user-login user))
	 (name (erc-server-user-full-name user))
	 (voice (and cuser (erc-channel-user-voice cuser)))
	 (op (and cuser (erc-channel-user-op cuser)))
	 (nick-str (concat (if op "@" "") (if voice "+" "") nick))
	 (finger (concat login (when (or login host) "@") host))
	 (sbtoken (list finger name info)))
    (if (or login host name info) ; we want to be expandable
	(speedbar-make-tag-line
	 'bracket ?+ 'erc-speedbar-expand-user sbtoken
	 nick-str nil sbtoken nil
	 indent)
      (when (equal exp-char ?-)
	(forward-line -1)
	(erc-speedbar-expand-user "+" (list finger name info) indent))
      (speedbar-make-tag-line
       'statictag ?? nil nil
       nick-str nil nil nil
       indent))))

(defun erc-speedbar-update-channel (buffer)
  "Update the speedbar information about a ERC buffer. The update
is only done when the channel is actually expanded already."
  ;; This is only a rude hack and doesn't care about multiserver usage
  ;; yet, consider this a brain storming, better ideas?
  (with-current-buffer speedbar-buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "^1: *.+. *"
				       (regexp-quote (buffer-name buffer)))
			       nil t)
	(beginning-of-line)
	(speedbar-delete-subblock 1)
	(erc-speedbar-expand-channel "+" buffer 1)))))

(defun erc-speedbar-expand-user (text token indent)
  (cond ((string-match "+" text)
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (let ((finger (nth 0 token))
		   (name (nth 1 token))
		   (info (nth 2 token)))
	       (when finger
		 (speedbar-make-tag-line
		  nil nil nil nil
		  finger nil nil nil
		  (1+ indent)))
	       (when name
		 (speedbar-make-tag-line
		  nil nil nil nil
		  name nil nil nil
		  (1+ indent)))
	       (when info
		 (speedbar-make-tag-line
		  nil nil nil nil
		  info nil nil nil
		  (1+ indent)))))))
	((string-match "-" text)
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun erc-speedbar-goto-buffer (text buffer indent)
  "When user clicks on TEXT, goto an ERC buffer.
The INDENT level is ignored."
  (if (featurep 'dframe)
      (progn
	(dframe-select-attached-frame speedbar-frame)
	(let ((bwin (get-buffer-window buffer 0)))
	  (if bwin
	      (progn
		(select-window bwin)
		(raise-frame (window-frame bwin)))
	    (if dframe-power-click
		(let ((pop-up-frames t))
		  (select-window (display-buffer buffer)))
	      (dframe-select-attached-frame speedbar-frame)
	      (switch-to-buffer buffer)))))
    (let ((bwin (get-buffer-window buffer 0)))
      (if bwin
	  (progn
	    (select-window bwin)
	    (raise-frame (window-frame bwin)))
	(if speedbar-power-click
	    (let ((pop-up-frames t)) (select-window (display-buffer buffer)))
	  (dframe-select-attached-frame speedbar-frame)
	  (switch-to-buffer buffer))))))

(defun erc-speedbar-line-text ()
  "Return the text for the item on the current line."
  (beginning-of-line)
  (when (re-search-forward "[]>] " nil t)
    (buffer-substring-no-properties (point) (point-at-eol))))

(defun erc-speedbar-item-info ()
  "Display information about the current buffer on the current line."
  (let ((data (speedbar-line-token))
	(txt (erc-speedbar-line-text)))
    (cond ((and data (listp data))
	   (message "%s: %s" txt (car data)))
	  ((bufferp data)
	   (message "Channel: %s" txt))
	  (t
	   (message "%s" txt)))))

(provide 'erc-speedbar)
;;; erc-speedbar.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

