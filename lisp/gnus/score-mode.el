;;; score-mode.el --- mode for editing Gnus score files

;; Copyright (C) 1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

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

(eval-when-compile (require 'cl))
(require 'mm-util)			; for mm-universal-coding-system
(require 'gnus-util)			; for gnus-pp, gnus-run-mode-hooks

(defvar gnus-score-edit-done-hook nil
  "*Hook run at the end of closing the score buffer.")

(defvar gnus-score-mode-hook nil
  "*Hook run in score mode buffers.")

(defvar gnus-score-menu-hook nil
  "*Hook run after creating the score mode menu.")

(defvar gnus-score-edit-exit-function nil
  "Function run on exit from the score buffer.")

(defvar gnus-score-mode-map nil)
(unless gnus-score-mode-map
  (setq gnus-score-mode-map (make-sparse-keymap))
  (set-keymap-parent gnus-score-mode-map emacs-lisp-mode-map)
  (define-key gnus-score-mode-map "\C-c\C-c" 'gnus-score-edit-exit)
  (define-key gnus-score-mode-map "\C-c\C-d" 'gnus-score-edit-insert-date)
  (define-key gnus-score-mode-map "\C-c\C-p" 'gnus-score-pretty-print))

(defvar score-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?| "w" table)
    table)
  "Syntax table used in score-mode buffers.")

;; We need this to cope with non-ASCII scoring.
(defvar score-mode-coding-system mm-universal-coding-system)

;;;###autoload
(defun gnus-score-mode ()
  "Mode for editing Gnus score files.
This mode is an extended emacs-lisp mode.

\\{gnus-score-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-score-mode-map)
  (gnus-score-make-menu-bar)
  (set-syntax-table score-mode-syntax-table)
  (setq major-mode 'gnus-score-mode)
  (setq mode-name "Score")
  (lisp-mode-variables nil)
  (make-local-variable 'gnus-score-edit-exit-function)
  (gnus-run-mode-hooks 'emacs-lisp-mode-hook 'gnus-score-mode-hook))

(defun gnus-score-make-menu-bar ()
  (unless (boundp 'gnus-score-menu)
    (easy-menu-define
     gnus-score-menu gnus-score-mode-map ""
     '("Score"
       ["Exit" gnus-score-edit-exit t]
       ["Insert date" gnus-score-edit-insert-date t]
       ["Format" gnus-score-pretty-print t]))
    (run-hooks 'gnus-score-menu-hook)))

(defun gnus-score-edit-insert-date ()
  "Insert date in numerical format."
  (interactive)
  (princ (time-to-days (current-time)) (current-buffer)))

(defun gnus-score-pretty-print ()
  "Format the current score file."
  (interactive)
  (goto-char (point-min))
  (let ((form (read (current-buffer))))
    (erase-buffer)
    (let ((emacs-lisp-mode-syntax-table score-mode-syntax-table))
      (gnus-pp form)))
  (goto-char (point-min)))

(defun gnus-score-edit-exit ()
  "Stop editing the score file."
  (interactive)
  (unless (file-exists-p (file-name-directory (buffer-file-name)))
    (make-directory (file-name-directory (buffer-file-name)) t))
  (let ((coding-system-for-write score-mode-coding-system))
    (save-buffer))
  (bury-buffer (current-buffer))
  (let ((buf (current-buffer)))
    (when gnus-score-edit-exit-function
      (funcall gnus-score-edit-exit-function))
    (when (eq buf (current-buffer))
      (switch-to-buffer (other-buffer (current-buffer))))))

(provide 'score-mode)

;;; score-mode.el ends here
