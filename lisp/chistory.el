;;; chistory.el --- list command history

;; Copyright (C) 1985, 2001-2012 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: convenience

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

;; This really has nothing to do with list-command-history per se, but
;; its a nice alternative to C-x ESC ESC (repeat-complex-command) and
;; functions as a lister if given no pattern.  It's not important
;; enough to warrant a file of its own.

;;; Code:

(defgroup chistory nil
  "List command history."
  :group 'keyboard)

;;;###autoload
(defun repeat-matching-complex-command (&optional pattern)
  "Edit and re-evaluate complex command with name matching PATTERN.
Matching occurrences are displayed, most recent first, until you select
a form for evaluation.  If PATTERN is empty (or nil), every form in the
command history is offered.  The form is placed in the minibuffer for
editing and the result is evaluated."
  (interactive "sRedo Command (regexp): ")
  (if pattern
      (if (string-match "[^ \t]" pattern)
	  (setq pattern (substring pattern (match-beginning 0)))
	(setq pattern nil)))
  (let ((history command-history)
	(temp)
	(what))
    (while (and history (not what))
      (setq temp (car history))
      (if (and (or (not pattern) (string-match pattern (symbol-name (car temp))))
	       (y-or-n-p (format "Redo %S? " temp)))
	  (setq what (car history))
	(setq history (cdr history))))
    (if (not what)
	(error "Command history exhausted")
      ;; Try to remove any useless command history element for this command.
      (if (eq (car (car command-history)) 'repeat-matching-complex-command)
	  (setq command-history (cdr command-history)))
      (edit-and-eval-command "Redo: " what))))

(defcustom default-command-history-filter-garbage
  '(command-history-mode
    list-command-history
    electric-command-history)
  "A list of symbols to be ignored by `default-command-history-filter'.
If that function is given a list whose car is an element of this list,
then it will return non-nil (indicating the list should be discarded from
the history).
Initially, all commands related to the command history are discarded."
  :type '(repeat symbol)
  :group 'chistory)

(defvar list-command-history-filter 'default-command-history-filter
  "Predicate to test which commands should be excluded from the history listing.
If non-nil, should be the name of a function of one argument.
It is passed each element of the command history when
\\[list-command-history] is called.  If the filter returns non-nil for
some element, that element is excluded from the history listing.  The
default filter removes commands associated with the command-history.")

(defun default-command-history-filter (frob)
  "Filter commands matching `default-command-history-filter-garbage' list
from the command history."
  (or (not (consp frob))
      (memq (car frob) default-command-history-filter-garbage)))

(defcustom list-command-history-max 32
  "If non-nil, maximum length of the listing produced by `list-command-history'."
  :type '(choice integer (const nil))
  :group 'chistory)

;;;###autoload
(defun list-command-history ()
  "List history of commands typed to minibuffer.
The number of commands listed is controlled by `list-command-history-max'.
Calls value of `list-command-history-filter' (if non-nil) on each history
element to judge if that element should be excluded from the list.

The buffer is left in Command History mode."
  (interactive)
  (with-output-to-temp-buffer
      "*Command History*"
    (let ((history command-history)
	  (buffer-read-only nil)
	  (count (or list-command-history-max -1)))
      (while (and (/= count 0) history)
	(if (and (bound-and-true-p list-command-history-filter)
		 (funcall list-command-history-filter (car history)))
	    nil
	  (setq count (1- count))
	  (prin1 (car history))
	  (terpri))
	(setq history (cdr history))))
    (with-current-buffer "*Command History*"
      (goto-char (point-min))
      (if (eobp)
	  (error "No command history")
	(command-history-mode)))))

(defvar command-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (suppress-keymap map)
    (define-key map "x" 'command-history-repeat)
    (define-key map "\n" 'next-line)
    (define-key map "\r" 'next-line)
    (define-key map "\177" 'previous-line)
    map)
  "Keymap for `command-history-mode'.")

(defun command-history-mode ()
  "Major mode for listing and repeating recent commands.

Keybindings:
\\{command-history-map}"
  (interactive)
  (Command-history-setup)
  (setq major-mode 'command-history-mode)
  (setq mode-name "Command History")
  (use-local-map command-history-map)
  (run-mode-hooks 'command-history-mode-hook))

(defun Command-history-setup ()
  (kill-all-local-variables)
  (use-local-map command-history-map)
  (lisp-mode-variables nil)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq buffer-read-only t))

(defcustom command-history-hook nil
  "If non-nil, its value is called on entry to `command-history-mode'."
  :type 'hook
  :group 'chistory)

(defun command-history-repeat ()
  "Repeat the command shown on the current line.
The buffer for that command is the previous current buffer."
  (interactive)
  (save-excursion
    (eval (prog1
	      (save-excursion
		(beginning-of-line)
		(read (current-buffer)))
	    (set-buffer
	     (car (cdr (buffer-list))))))))

;;;###autoload
(defun command-history ()
  "Examine commands from `command-history' in a buffer.
The number of commands listed is controlled by `list-command-history-max'.
The command history is filtered by `list-command-history-filter' if non-nil.
Use \\<command-history-map>\\[command-history-repeat] to repeat the command on the current line.

Otherwise much like Emacs-Lisp Mode except that there is no self-insertion
and digits provide prefix arguments.  Tab does not indent.
\\{command-history-map}

This command always recompiles the Command History listing
and runs the normal hook `command-history-hook'."
  (interactive)
  (list-command-history)
  (pop-to-buffer "*Command History*")
  (run-hooks 'command-history-hook))

(provide 'chistory)

;;; chistory.el ends here
