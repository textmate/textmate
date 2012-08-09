;;; ebuff-menu.el --- electric-buffer-list mode

;; Copyright (C) 1985-1986, 1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@ai.mit.edu>
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

;; Who says one can't have typeout windows in GNU Emacs?   The entry
;; point, `electric-buffer-list' works like ^r select buffer from the
;; ITS Emacs lunar or tmacs libraries.

;;; Code:

(require 'electric)

;; this depends on the format of list-buffers (from src/buffer.c) and
;; on stuff in lisp/buff-menu.el

(defvar electric-buffer-menu-mode-map
  (let ((map (make-keymap)))
    (fillarray (car (cdr map)) 'Electric-buffer-menu-undefined)
    (define-key map "\e" nil)
    (define-key map "\C-z" 'suspend-frame)
    (define-key map "v" 'Electric-buffer-menu-mode-view-buffer)
    (define-key map (char-to-string help-char) 'Helper-help)
    (define-key map "?" 'Helper-describe-bindings)
    (define-key map "\C-c" nil)
    (define-key map "\C-c\C-c" 'Electric-buffer-menu-quit)
    (define-key map "\C-]" 'Electric-buffer-menu-quit)
    (define-key map "q" 'Electric-buffer-menu-quit)
    (define-key map " " 'Electric-buffer-menu-select)
    (define-key map "\C-m" 'Electric-buffer-menu-select)
    (define-key map "\C-l" 'recenter)
    (define-key map "s" 'Buffer-menu-save)
    (define-key map "d" 'Buffer-menu-delete)
    (define-key map "k" 'Buffer-menu-delete)
    (define-key map "\C-d" 'Buffer-menu-delete-backwards)
    ;; (define-key map "\C-k" 'Buffer-menu-delete)
    (define-key map "\177" 'Buffer-menu-backup-unmark)
    (define-key map "~" 'Buffer-menu-not-modified)
    (define-key map "u" 'Buffer-menu-unmark)
    (let ((i ?0))
      (while (<= i ?9)
	(define-key map (char-to-string i) 'digit-argument)
	(define-key map (concat "\e" (char-to-string i)) 'digit-argument)
	(setq i (1+ i))))
    (define-key map "-" 'negative-argument)
    (define-key map "\e-" 'negative-argument)
    (define-key map "m" 'Buffer-menu-mark)
    (define-key map "\C-u" 'universal-argument)
    (define-key map "\C-p" 'previous-line)
    (define-key map "\C-n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "\C-v" 'scroll-up-command)
    (define-key map "\ev" 'scroll-down-command)
    (define-key map ">" 'scroll-right)
    (define-key map "<" 'scroll-left)
    (define-key map "\e\C-v" 'scroll-other-window)
    (define-key map "\e>" 'end-of-buffer)
    (define-key map "\e<" 'beginning-of-buffer)
    (define-key map "\e\e" nil)
    (define-key map "\e\e\e" 'Electric-buffer-menu-quit)
    ;; This binding prevents the "escape => ESC" function-key-map mapping from
    ;; kicking in!
    ;; (define-key map [escape escape escape] 'Electric-buffer-menu-quit)
    (define-key map [mouse-2] 'Electric-buffer-menu-mouse-select)
    map))

(defvar electric-buffer-menu-mode-hook nil
  "Normal hook run by `electric-buffer-list'.")

;;;###autoload
(defun electric-buffer-list (arg)
  "Pop up a buffer describing the set of Emacs buffers.
Vaguely like ITS lunar select buffer; combining typeoutoid buffer
listing with menuoid buffer selection.

If the very next character typed is a space then the buffer list
window disappears.  Otherwise, one may move around in the buffer list
window, marking buffers to be selected, saved or deleted.

To exit and select a new buffer, type a space when the cursor is on
the appropriate line of the buffer-list window.  Other commands are
much like those of `Buffer-menu-mode'.

Run hooks in `electric-buffer-menu-mode-hook' on entry.

\\{electric-buffer-menu-mode-map}"
  (interactive "P")
  (let (select buffer)
    (save-window-excursion
      (setq buffer (list-buffers-noselect arg))
      (Electric-pop-up-window buffer)
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (Electric-buffer-menu-mode)
	    (electric-buffer-update-highlight)
	    (setq select
		  (catch 'electric-buffer-menu-select
		    (message "<<< Press Return to bury the buffer list >>>")
		    (if (eq (setq unread-command-events (list (read-event)))
			    ?\s)
			(progn (setq unread-command-events nil)
			       (throw 'electric-buffer-menu-select nil)))
		    (let ((start-point (point))
			  (first (progn (goto-char (point-min))
					(unless Buffer-menu-use-header-line
					  (forward-line 2))
					(point)))
			  (last (progn (goto-char (point-max))
				       (forward-line -1)
				       (point)))
			  (goal-column 0))
		      ;; Use start-point if it is meaningful.
		      (goto-char (if (or (< start-point first)
					 (> start-point last))
				     first
				   start-point))
		      (Electric-command-loop 'electric-buffer-menu-select
					     nil
					     t
					     'electric-buffer-menu-looper
					     (cons first last))))))
	(set-buffer buffer)
	(Buffer-menu-mode)
	(bury-buffer)                ;Get rid of window, if dedicated.
	(message "")))
    (if select
	(progn (set-buffer buffer)
	       (let ((opoint (point-marker)))
		 (Buffer-menu-execute)
		 (goto-char (point-min))
		 (if (prog1 (search-forward "\n>" nil t)
		       (goto-char opoint) (set-marker opoint nil))
		     (Buffer-menu-select)
		     (switch-to-buffer (Buffer-menu-buffer t))))))))

(defun electric-buffer-menu-looper (state condition)
  (cond ((and condition
	      (not (memq (car condition) '(buffer-read-only
					   end-of-buffer
					   beginning-of-buffer))))
	 (signal (car condition) (cdr condition)))
	((< (point) (car state))
	 (goto-char (point-min))
	 (unless Buffer-menu-use-header-line
	   (forward-line 2)))
	((> (point) (cdr state))
	 (goto-char (point-max))
	 (forward-line -1)
	 (if (pos-visible-in-window-p (point-max))
	     (recenter -1))))
  (electric-buffer-update-highlight))

(defvar Helper-return-blurb)

(put 'Electric-buffer-menu-mode 'mode-class 'special)
(defun Electric-buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<electric-buffer-menu-mode-map>
\\[keyboard-quit] or \\[Electric-buffer-menu-quit] -- exit buffer menu, returning to previous window and buffer
  configuration.  If the very first character typed is a space, it
  also has this effect.
\\[Electric-buffer-menu-select] -- select buffer of line point is on.
  Also show buffers marked with m in other windows,
  deletes buffers marked with \"D\", and saves those marked with \"S\".
\\[Buffer-menu-mark] -- mark buffer to be displayed.
\\[Buffer-menu-not-modified] -- clear modified-flag on that buffer.
\\[Buffer-menu-save] -- mark that buffer to be saved.
\\[Buffer-menu-delete] or \\[Buffer-menu-delete-backwards] -- mark that buffer to be deleted.
\\[Buffer-menu-unmark] -- remove all kinds of marks from current line.
\\[Electric-buffer-menu-mode-view-buffer] -- view buffer, returning when done.
\\[Buffer-menu-backup-unmark] -- back up a line and remove marks.

\\{electric-buffer-menu-mode-map}

Entry to this mode via command `electric-buffer-list' calls the value of
`electric-buffer-menu-mode-hook'."
  (let ((saved header-line-format))
    (kill-all-local-variables)
    (setq header-line-format saved))
  (use-local-map electric-buffer-menu-mode-map)
  (setq mode-name "Electric Buffer Menu")
  (setq mode-line-buffer-identification "Electric Buffer List")
  (make-local-variable 'Helper-return-blurb)
  (setq Helper-return-blurb "return to buffer editing")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'Electric-buffer-menu-mode)
  (goto-char (point-min))
  (if (search-forward "\n." nil t) (forward-char -1))
  (run-mode-hooks 'electric-buffer-menu-mode-hook))

;; generally the same as Buffer-menu-mode-map
;;  (except we don't indirect to global-map)
(put 'Electric-buffer-menu-undefined 'suppress-keymap t)


(defun Electric-buffer-menu-exit ()
  (interactive)
  (setq unread-command-events (listify-key-sequence (this-command-keys)))
  ;; for robustness
  (condition-case ()
      (throw 'electric-buffer-menu-select nil)
    (error (Buffer-menu-mode)
	   (other-buffer))))

(defun Electric-buffer-menu-select ()
  "Leave Electric Buffer Menu, selecting buffers and executing changes.
Save buffers marked \"S\".  Delete buffers marked \"K\".
Select buffer at point and display buffers marked \">\" in other windows."
  (interactive)
  (throw 'electric-buffer-menu-select (point)))

(defun Electric-buffer-menu-mouse-select (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (set-buffer (window-buffer (selected-window)))
  (goto-char (posn-point (event-end event)))
  (throw 'electric-buffer-menu-select (point)))

(defun Electric-buffer-menu-quit ()
  "Leave Electric Buffer Menu, restoring previous window configuration.
Skip execution of select, save, and delete commands."
  (interactive)
  (throw 'electric-buffer-menu-select nil))

(defun Electric-buffer-menu-undefined ()
  (interactive)
  (ding)
  (message "%s"
	   (if (and (eq (key-binding "\C-c\C-c") 'Electric-buffer-menu-quit)
		    (eq (key-binding " ") 'Electric-buffer-menu-select)
		    (eq (key-binding (char-to-string help-char)) 'Helper-help)
		    (eq (key-binding "?") 'Helper-describe-bindings))
	       (substitute-command-keys "Type C-c C-c to exit, Space to select, \\[Helper-help] for help, ? for commands")
	     (substitute-command-keys "\
Type \\[Electric-buffer-menu-quit] to exit, \
\\[Electric-buffer-menu-select] to select, \
\\[Helper-help] for help, \\[Helper-describe-bindings] for commands.")))
  (sit-for 4))

(defun Electric-buffer-menu-mode-view-buffer ()
  "View buffer on current line in Electric Buffer Menu.
Return to Electric Buffer Menu when done."
  (interactive)
  (let ((bufnam (Buffer-menu-buffer nil)))
    (if bufnam
	(view-buffer bufnam)
      (ding)
      (message "Buffer %s does not exist!" bufnam)
      (sit-for 4))))

(defvar electric-buffer-overlay nil)
(defun electric-buffer-update-highlight ()
  (when (eq major-mode 'Electric-buffer-menu-mode)
    ;; Make sure we have an overlay to use.
    (or electric-buffer-overlay
	(progn
	  (make-local-variable 'electric-buffer-overlay)
	  (setq electric-buffer-overlay (make-overlay (point) (point)))))
    (move-overlay electric-buffer-overlay
		  (line-beginning-position)
		  (line-end-position))
    (overlay-put electric-buffer-overlay 'face 'highlight)))

(provide 'ebuff-menu)

;;; ebuff-menu.el ends here
