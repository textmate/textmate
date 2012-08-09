;;; eudc-hotlist.el --- hotlist management for EUDC

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;; Maintainer: Pavel Janík <Pavel@Janik.cz>
;; Keywords: comm
;; Package: eudc

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

;;; Usage:
;;    See the corresponding info file

;;; Code:

(require 'eudc)

(defvar eudc-hotlist-menu nil)
(defvar eudc-hotlist-list-beginning nil)

(defvar eudc-hotlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'eudc-hotlist-add-server)
    (define-key map "d" 'eudc-hotlist-delete-server)
    (define-key map "s" 'eudc-hotlist-select-server)
    (define-key map "t" 'eudc-hotlist-transpose-servers)
    (define-key map "q" 'eudc-hotlist-quit-edit)
    (define-key map "x" 'kill-this-buffer)
    map))

(defun eudc-hotlist-mode ()
  "Major mode used to edit the hotlist of servers.

These are the special commands of this mode:
    a -- Add a new server to the list.
    d -- Delete the server at point from the list.
    s -- Select the server at point.
    t -- Transpose the server at point and the previous one
    q -- Commit the changes and quit.
    x -- Quit without committing the changes."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'eudc-hotlist-mode)
  (setq mode-name "EUDC-Servers")
  (use-local-map eudc-hotlist-mode-map)
  (when (featurep 'xemacs)
    (setq mode-popup-menu eudc-hotlist-menu)
    (when (featurep 'menubar)
      (set-buffer-menubar current-menubar)
      (add-submenu nil (cons "EUDC-Hotlist" (cdr (cdr eudc-hotlist-menu))))))
  (setq buffer-read-only t)
  (run-mode-hooks 'eudc-hotlist-mode-hook))

;;;###autoload
(defun eudc-edit-hotlist ()
  "Edit the hotlist of directory servers in a specialized buffer."
  (interactive)
  (let ((proto-col 10)
	gap)
    (switch-to-buffer (get-buffer-create "*EUDC Servers*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (mapc (function
	   (lambda (entry)
	     (setq proto-col (max (length (car entry)) proto-col))))
	  eudc-server-hotlist)
    (setq proto-col (+ 3 proto-col))
    (setq gap (make-string (- proto-col 6) ?\ ))
    (insert "              EUDC Servers\n"
	    "              ============\n"
	    "\n"
	    "Server" gap "Protocol\n"
	    "------" gap "--------\n"
	    "\n")
    (setq eudc-hotlist-list-beginning (point))
    (mapc (lambda (entry)
	     (insert (car entry))
	     (indent-to proto-col)
	     (insert (symbol-name (cdr entry)) "\n"))
	  eudc-server-hotlist)
  (eudc-hotlist-mode)))

(defun eudc-hotlist-add-server ()
  "Add a new server to the list after current one."
  (interactive)
  (if (not (eq major-mode 'eudc-hotlist-mode))
      (error "Not in a EUDC hotlist edit buffer"))
  (let ((server (read-from-minibuffer "Server: "))
	(protocol (completing-read "Protocol: "
				   (mapcar (lambda (elt)
					      (cons (symbol-name elt)
						    elt))
					   eudc-known-protocols)))
	(buffer-read-only nil))
    (if (not (eobp))
	(forward-line 1))
    (insert server)
    (indent-to 30)
    (insert protocol "\n")))

(defun eudc-hotlist-delete-server ()
  "Delete the server at point from the list."
  (interactive)
  (if (not (eq major-mode 'eudc-hotlist-mode))
      (error "Not in a EUDC hotlist edit buffer"))
  (let ((buffer-read-only nil))
    (save-excursion
      (beginning-of-line)
      (if (and (>= (point) eudc-hotlist-list-beginning)
	       (looking-at "^\\([-.a-zA-Z:0-9]+\\)[ \t]+\\([a-zA-Z]+\\)"))
	  (kill-line 1)
	(error "No server on this line")))))

(defun eudc-hotlist-quit-edit ()
  "Quit the hotlist editing mode and save changes to the hotlist."
  (interactive)
  (if (not (eq major-mode 'eudc-hotlist-mode))
      (error "Not in a EUDC hotlist edit buffer"))
  (let (hotlist)
    (goto-char eudc-hotlist-list-beginning)
    (while (looking-at "^\\([-.a-zA-Z:0-9]+\\)[ \t]+\\([a-zA-Z]+\\)")
      (setq hotlist (cons (cons (match-string 1)
				(intern (match-string 2)))
			  hotlist))
      (forward-line 1))
    (if (not (looking-at "^[ \t]*$"))
	(error "Malformed entry in hotlist, discarding edits"))
    (setq eudc-server-hotlist (nreverse hotlist))
    (eudc-install-menu)
    (eudc-save-options)
    (kill-this-buffer)))

(defun eudc-hotlist-select-server ()
  "Select the server at point as the current server."
  (interactive)
  (if (not (eq major-mode 'eudc-hotlist-mode))
      (error "Not in a EUDC hotlist edit buffer"))
  (save-excursion
    (beginning-of-line)
    (if (and (>= (point) eudc-hotlist-list-beginning)
	     (looking-at "^\\([-.a-zA-Z:0-9]+\\)[ \t]+\\([a-zA-Z]+\\)"))
	(progn
	  (eudc-set-server (match-string 1) (intern (match-string 2)))
	  (message "Current directory server is %s (%s)" eudc-server eudc-protocol))
      (error "No server on this line"))))

(defun eudc-hotlist-transpose-servers ()
  "Swap the order of the server with the previous one in the list."
  (interactive)
  (if (not (eq major-mode 'eudc-hotlist-mode))
      (error "Not in a EUDC hotlist edit buffer"))
  (let ((buffer-read-only nil))
    (save-excursion
      (beginning-of-line)
      (if (and (>= (point) eudc-hotlist-list-beginning)
	       (looking-at "^\\([-.a-zA-Z:0-9]+\\)[ \t]+\\([a-zA-Z]+\\)")
	       (progn
		 (forward-line -1)
		 (looking-at "^\\([-.a-zA-Z:0-9]+\\)[ \t]+\\([a-zA-Z]+\\)")))
	  (progn
	    (forward-line 1)
	    (transpose-lines 1))))))

(defconst eudc-hotlist-menu
  '("EUDC Hotlist Edit"
    ["---" nil nil]
    ["Add New Server" eudc-hotlist-add-server t]
    ["Delete Server" eudc-hotlist-delete-server t]
    ["Select Server" eudc-hotlist-select-server t]
    ["Transpose Servers" eudc-hotlist-transpose-servers t]
    ["Save and Quit" eudc-hotlist-quit-edit t]
    ["Exit without Saving" kill-this-buffer t]))

(when (not (featurep 'xemacs))
  (easy-menu-define eudc-hotlist-emacs-menu
    eudc-hotlist-mode-map
    ""
    eudc-hotlist-menu))

;;; eudc-hotlist.el ends here
