;;; gnus-eform.el --- a mode for editing forms for Gnus

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

(require 'gnus)
(require 'gnus-win)

;;;
;;; Editing forms
;;;

(defgroup gnus-edit-form nil
  "A mode for editing forms."
  :group 'gnus)

(defcustom gnus-edit-form-mode-hook nil
  "Hook run in `gnus-edit-form-mode' buffers."
  :group 'gnus-edit-form
  :type 'hook)

(defcustom gnus-edit-form-menu-hook nil
  "Hook run when creating menus in `gnus-edit-form-mode' buffers."
  :group 'gnus-edit-form
  :type 'hook)

;;; Internal variables

(defvar gnus-edit-form-buffer "*Gnus edit form*")
(defvar gnus-edit-form-done-function nil)

(defvar gnus-edit-form-mode-map nil)
(unless gnus-edit-form-mode-map
  (setq gnus-edit-form-mode-map (make-sparse-keymap))
  (set-keymap-parent gnus-edit-form-mode-map emacs-lisp-mode-map)
  (gnus-define-keys gnus-edit-form-mode-map
    "\C-c\C-c" gnus-edit-form-done
    "\C-c\C-k" gnus-edit-form-exit))

(defun gnus-edit-form-make-menu-bar ()
  (unless (boundp 'gnus-edit-form-menu)
    (easy-menu-define
     gnus-edit-form-menu gnus-edit-form-mode-map ""
     '("Edit Form"
       ["Exit and save changes" gnus-edit-form-done t]
       ["Exit" gnus-edit-form-exit t]))
    (gnus-run-hooks 'gnus-edit-form-menu-hook)))

(defun gnus-edit-form-mode ()
  "Major mode for editing forms.
It is a slightly enhanced emacs-lisp-mode.

\\{gnus-edit-form-mode-map}"
  (interactive)
  (when (gnus-visual-p 'group-menu 'menu)
    (gnus-edit-form-make-menu-bar))
  (kill-all-local-variables)
  (setq major-mode 'gnus-edit-form-mode)
  (setq mode-name "Edit Form")
  (use-local-map gnus-edit-form-mode-map)
  (make-local-variable 'gnus-edit-form-done-function)
  (make-local-variable 'gnus-prev-winconf)
  (gnus-run-mode-hooks 'gnus-edit-form-mode-hook))

(defun gnus-edit-form (form documentation exit-func &optional layout)
  "Edit FORM in a new buffer.
Call EXIT-FUNC on exit.  Display DOCUMENTATION in the beginning
of the buffer.
The optional LAYOUT overrides the `edit-form' window layout."
  (let ((winconf (current-window-configuration)))
    (set-buffer (gnus-get-buffer-create gnus-edit-form-buffer))
    (gnus-configure-windows (or layout 'edit-form))
    (gnus-edit-form-mode)
    (setq gnus-prev-winconf winconf)
    (setq gnus-edit-form-done-function exit-func)
    (erase-buffer)
    (insert documentation)
    (unless (bolp)
      (insert "\n"))
    (goto-char (point-min))
    (while (not (eobp))
      (insert ";;; ")
      (forward-line 1))
    (insert ";; Type `C-c C-c' after you've finished editing.\n")
    (insert "\n")
    (let ((p (point)))
      (gnus-pp form)
      (insert "\n")
      (goto-char p))))

(defun gnus-edit-form-done ()
  "Update changes and kill the current buffer."
  (interactive)
  (goto-char (point-min))
  (let ((form (condition-case nil
		  (read (current-buffer))
		(end-of-file nil)))
	(func gnus-edit-form-done-function))
    (gnus-edit-form-exit)
    (funcall func form)))

(defun gnus-edit-form-exit ()
  "Kill the current buffer."
  (interactive)
  (let ((winconf gnus-prev-winconf))
    (kill-buffer (current-buffer))
    (set-window-configuration winconf)))

(provide 'gnus-eform)

;;; gnus-eform.el ends here
