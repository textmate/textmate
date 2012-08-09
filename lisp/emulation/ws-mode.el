;;; ws-mode.el --- WordStar emulation mode for GNU Emacs

;; Copyright (C) 1991, 2001-2012  Free Software Foundation, Inc.

;; Author: Juergen Nickelsen <nickel@cs.tu-berlin.de>
;; Version: 0.7
;; Keywords: emulations

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

;; This emulates WordStar, with a major mode.

;;; Code:
(defvar wordstar-C-k-map
  (let ((map (make-keymap)))
    (define-key map " " ())
    (define-key map "0" 'ws-set-marker-0)
    (define-key map "1" 'ws-set-marker-1)
    (define-key map "2" 'ws-set-marker-2)
    (define-key map "3" 'ws-set-marker-3)
    (define-key map "4" 'ws-set-marker-4)
    (define-key map "5" 'ws-set-marker-5)
    (define-key map "6" 'ws-set-marker-6)
    (define-key map "7" 'ws-set-marker-7)
    (define-key map "8" 'ws-set-marker-8)
    (define-key map "9" 'ws-set-marker-9)
    (define-key map "b" 'ws-begin-block)
    (define-key map "\C-b" 'ws-begin-block)
    (define-key map "c" 'ws-copy-block)
    (define-key map "\C-c" 'ws-copy-block)
    (define-key map "d" 'save-buffers-kill-emacs)
    (define-key map "\C-d" 'save-buffers-kill-emacs)
    (define-key map "f" 'find-file)
    (define-key map "\C-f" 'find-file)
    (define-key map "h" 'ws-show-markers)
    (define-key map "\C-h" 'ws-show-markers)
    (define-key map "i" 'ws-indent-block)
    (define-key map "\C-i" 'ws-indent-block)
    (define-key map "k" 'ws-end-block)
    (define-key map "\C-k" 'ws-end-block)
    (define-key map "p" 'ws-print-block)
    (define-key map "\C-p" 'ws-print-block)
    (define-key map "q" 'kill-emacs)
    (define-key map "\C-q" 'kill-emacs)
    (define-key map "r" 'insert-file)
    (define-key map "\C-r" 'insert-file)
    (define-key map "s" 'save-some-buffers)
    (define-key map "\C-s" 'save-some-buffers)
    (define-key map "t" 'ws-mark-word)
    (define-key map "\C-t" 'ws-mark-word)
    (define-key map "u" 'ws-exdent-block)
    (define-key map "\C-u" 'keyboard-quit)
    (define-key map "v" 'ws-move-block)
    (define-key map "\C-v" 'ws-move-block)
    (define-key map "w" 'ws-write-block)
    (define-key map "\C-w" 'ws-write-block)
    (define-key map "x" 'save-buffers-kill-emacs)
    (define-key map "\C-x" 'save-buffers-kill-emacs)
    (define-key map "y" 'ws-delete-block)
    (define-key map "\C-y" 'ws-delete-block)
    map)
  "")

(defvar wordstar-C-o-map
  (let ((map (make-keymap)))
    (define-key map " " ())
    (define-key map "c" 'wordstar-center-line)
    (define-key map "\C-c" 'wordstar-center-line)
    (define-key map "b" 'switch-to-buffer)
    (define-key map "\C-b" 'switch-to-buffer)
    (define-key map "j" 'justify-current-line)
    (define-key map "\C-j" 'justify-current-line)
    (define-key map "k" 'kill-buffer)
    (define-key map "\C-k" 'kill-buffer)
    (define-key map "l" 'list-buffers)
    (define-key map "\C-l" 'list-buffers)
    (define-key map "m" 'auto-fill-mode)
    (define-key map "\C-m" 'auto-fill-mode)
    (define-key map "r" 'set-fill-column)
    (define-key map "\C-r" 'set-fill-column)
    (define-key map "\C-u" 'keyboard-quit)
    (define-key map "wd" 'delete-other-windows)
    (define-key map "wh" 'split-window-right)
    (define-key map "wo" 'other-window)
    (define-key map "wv" 'split-window-below)
    map)
  "")

(defvar wordstar-C-q-map
  (let ((map (make-keymap)))
    (define-key map " " ())
    (define-key map "0" 'ws-find-marker-0)
    (define-key map "1" 'ws-find-marker-1)
    (define-key map "2" 'ws-find-marker-2)
    (define-key map "3" 'ws-find-marker-3)
    (define-key map "4" 'ws-find-marker-4)
    (define-key map "5" 'ws-find-marker-5)
    (define-key map "6" 'ws-find-marker-6)
    (define-key map "7" 'ws-find-marker-7)
    (define-key map "8" 'ws-find-marker-8)
    (define-key map "9" 'ws-find-marker-9)
    (define-key map "a" 'ws-query-replace)
    (define-key map "\C-a" 'ws-query-replace)
    (define-key map "b" 'ws-goto-block-begin)
    (define-key map "\C-b" 'ws-goto-block-begin)
    (define-key map "c" 'end-of-buffer)
    (define-key map "\C-c" 'end-of-buffer)
    (define-key map "d" 'end-of-line)
    (define-key map "\C-d" 'end-of-line)
    (define-key map "f" 'ws-search)
    (define-key map "\C-f" 'ws-search)
    (define-key map "k" 'ws-goto-block-end)
    (define-key map "\C-k" 'ws-goto-block-end)
    (define-key map "l" 'ws-undo)
    (define-key map "\C-l" 'ws-undo)
    (define-key map "p" 'ws-last-cursorp)
    (define-key map "\C-p" 'ws-last-cursorp)
    (define-key map "r" 'beginning-of-buffer)
    (define-key map "\C-r" 'beginning-of-buffer)
    (define-key map "s" 'beginning-of-line)
    (define-key map "\C-s" 'beginning-of-line)
    (define-key map "\C-u" 'keyboard-quit)
    (define-key map "w" 'ws-last-error)
    (define-key map "\C-w" 'ws-last-error)
    (define-key map "y" 'ws-kill-eol)
    (define-key map "\C-y" 'ws-kill-eol)
    (define-key map "\177" 'ws-kill-bol)
    map)
  "")

(defvar wordstar-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-a" 'backward-word)
    (define-key map "\C-b" 'fill-paragraph)
    (define-key map "\C-c" 'scroll-up-command)
    (define-key map "\C-d" 'forward-char)
    (define-key map "\C-e" 'previous-line)
    (define-key map "\C-f" 'forward-word)
    (define-key map "\C-g" 'delete-char)
    (define-key map "\C-h" 'backward-char)
    (define-key map "\C-i" 'indent-for-tab-command)
    (define-key map "\C-j" 'help-for-help)
    (define-key map "\C-k" wordstar-C-k-map)
    (define-key map "\C-l" 'ws-repeat-search)
    (define-key map "\C-n" 'open-line)
    (define-key map "\C-o" wordstar-C-o-map)
    (define-key map "\C-p" 'quoted-insert)
    (define-key map "\C-q" wordstar-C-q-map)
    (define-key map "\C-r" 'scroll-down-command)
    (define-key map "\C-s" 'backward-char)
    (define-key map "\C-t" 'kill-word)
    (define-key map "\C-u" 'keyboard-quit)
    (define-key map "\C-v" 'overwrite-mode)
    (define-key map "\C-w" 'scroll-down-line)
    (define-key map "\C-x" 'next-line)
    (define-key map "\C-y" 'kill-complete-line)
    (define-key map "\C-z" 'scroll-up-line)
    map)
  "")

;; wordstar-C-j-map not yet implemented
(defvar wordstar-C-j-map nil "")


(put 'wordstar-mode 'mode-class 'special)

;;;###autoload
(defun wordstar-mode ()
  "Major mode with WordStar-like key bindings.

BUGS:
 - Help menus with WordStar commands (C-j just calls help-for-help)
   are not implemented
 - Options for search and replace
 - Show markers (C-k h) is somewhat strange
 - Search and replace (C-q a) is only available in forward direction

No key bindings beginning with ESC are installed, they will work
Emacs-like.

The key bindings are:

  C-a		backward-word
  C-b		fill-paragraph
  C-c		scroll-up-line
  C-d		forward-char
  C-e		previous-line
  C-f		forward-word
  C-g		delete-char
  C-h		backward-char
  C-i		indent-for-tab-command
  C-j		help-for-help
  C-k		ordstar-C-k-map
  C-l		ws-repeat-search
  C-n		open-line
  C-p		quoted-insert
  C-r		scroll-down-line
  C-s		backward-char
  C-t		kill-word
  C-u		keyboard-quit
  C-v		overwrite-mode
  C-w		scroll-down
  C-x		next-line
  C-y		kill-complete-line
  C-z		scroll-up

  C-k 0		ws-set-marker-0
  C-k 1		ws-set-marker-1
  C-k 2		ws-set-marker-2
  C-k 3		ws-set-marker-3
  C-k 4		ws-set-marker-4
  C-k 5		ws-set-marker-5
  C-k 6		ws-set-marker-6
  C-k 7		ws-set-marker-7
  C-k 8		ws-set-marker-8
  C-k 9		ws-set-marker-9
  C-k b		ws-begin-block
  C-k c		ws-copy-block
  C-k d		save-buffers-kill-emacs
  C-k f		find-file
  C-k h		ws-show-markers
  C-k i		ws-indent-block
  C-k k		ws-end-block
  C-k p		ws-print-block
  C-k q		kill-emacs
  C-k r		insert-file
  C-k s		save-some-buffers
  C-k t		ws-mark-word
  C-k u		ws-exdent-block
  C-k C-u	keyboard-quit
  C-k v		ws-move-block
  C-k w		ws-write-block
  C-k x		kill-emacs
  C-k y		ws-delete-block

  C-o c		wordstar-center-line
  C-o b		switch-to-buffer
  C-o j		justify-current-line
  C-o k		kill-buffer
  C-o l		list-buffers
  C-o m		auto-fill-mode
  C-o r		set-fill-column
  C-o C-u	keyboard-quit
  C-o wd	delete-other-windows
  C-o wh	split-window-right
  C-o wo	other-window
  C-o wv	split-window-below

  C-q 0		ws-find-marker-0
  C-q 1		ws-find-marker-1
  C-q 2		ws-find-marker-2
  C-q 3		ws-find-marker-3
  C-q 4		ws-find-marker-4
  C-q 5		ws-find-marker-5
  C-q 6		ws-find-marker-6
  C-q 7		ws-find-marker-7
  C-q 8		ws-find-marker-8
  C-q 9		ws-find-marker-9
  C-q a		ws-query-replace
  C-q b		ws-to-block-begin
  C-q c		end-of-buffer
  C-q d		end-of-line
  C-q f		ws-search
  C-q k		ws-to-block-end
  C-q l		ws-undo
  C-q p		ws-last-cursorp
  C-q r		beginning-of-buffer
  C-q C-u	keyboard-quit
  C-q w		ws-last-error
  C-q y		ws-kill-eol
  C-q DEL	ws-kill-bol
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map wordstar-mode-map)
  (setq mode-name "WordStar")
  (setq major-mode 'wordstar-mode)
  (run-mode-hooks 'wordstar-mode-hook))


(defun wordstar-center-paragraph ()
  "Center each line in the paragraph at or after point.
See `wordstar-center-line' for more info."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (wordstar-center-region (point) end))))

(defun wordstar-center-region (from to)
  "Center each line starting in the region.
See `wordstar-center-line' for more info."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(wordstar-center-line)
	(forward-line 1)))))

(defun wordstar-center-line ()
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation to match
the distance between the end of the text and `fill-column'."
  (interactive)
  (save-excursion
    (let (line-length)
      (beginning-of-line)
      (delete-horizontal-space)
      (end-of-line)
      (delete-horizontal-space)
      (setq line-length (current-column))
      (beginning-of-line)
      (indent-to
       (+ left-margin
	  (/ (- fill-column left-margin line-length) 2))))))

;;;;;;;;;;;
;; wordstar special variables:

(defvar ws-marker-0 nil "Position marker 0 in WordStar mode.")
(defvar ws-marker-1 nil "Position marker 1 in WordStar mode.")
(defvar ws-marker-2 nil "Position marker 2 in WordStar mode.")
(defvar ws-marker-3 nil "Position marker 3 in WordStar mode.")
(defvar ws-marker-4 nil "Position marker 4 in WordStar mode.")
(defvar ws-marker-5 nil "Position marker 5 in WordStar mode.")
(defvar ws-marker-6 nil "Position marker 6 in WordStar mode.")
(defvar ws-marker-7 nil "Position marker 7 in WordStar mode.")
(defvar ws-marker-8 nil "Position marker 8 in WordStar mode.")
(defvar ws-marker-9 nil "Position marker 9 in WordStar mode.")

(defvar ws-block-begin-marker nil "Beginning of \"Block\" in WordStar mode.")
(defvar ws-block-end-marker nil "End of \"Block\" in WordStar mode.")

(defvar ws-search-string nil "String of last search in WordStar mode.")
(defvar ws-search-direction t
  "Direction of last search in WordStar mode. t if forward, nil if backward.")

(defvar ws-last-cursorposition nil
  "Position before last search etc. in WordStar mode.")

(defvar ws-last-errormessage nil
  "Last error message issued by a WordStar mode function.")

;;;;;;;;;;;
;; wordstar special functions:

(defun ws-error (string)
  "Report error of a WordStar special function. Error message is saved
in ws-last-errormessage for recovery with C-q w."
  (setq ws-last-errormessage string)
  (error string))

(defun ws-set-marker-0 ()
  "In WordStar mode: Set marker 0 to current cursor position."
  (interactive)
  (setq ws-marker-0 (point-marker))
  (message "Marker 0 set"))

(defun ws-set-marker-1 ()
  "In WordStar mode: Set marker 1 to current cursor position."
  (interactive)
  (setq ws-marker-1 (point-marker))
  (message "Marker 1 set"))

(defun ws-set-marker-2 ()
  "In WordStar mode: Set marker 2 to current cursor position."
  (interactive)
  (setq ws-marker-2 (point-marker))
  (message "Marker 2 set"))

(defun ws-set-marker-3 ()
  "In WordStar mode: Set marker 3 to current cursor position."
  (interactive)
  (setq ws-marker-3 (point-marker))
  (message "Marker 3 set"))

(defun ws-set-marker-4 ()
  "In WordStar mode: Set marker 4 to current cursor position."
  (interactive)
  (setq ws-marker-4 (point-marker))
  (message "Marker 4 set"))

(defun ws-set-marker-5 ()
  "In WordStar mode: Set marker 5 to current cursor position."
  (interactive)
  (setq ws-marker-5 (point-marker))
  (message "Marker 5 set"))

(defun ws-set-marker-6 ()
  "In WordStar mode: Set marker 6 to current cursor position."
  (interactive)
  (setq ws-marker-6 (point-marker))
  (message "Marker 6 set"))

(defun ws-set-marker-7 ()
  "In WordStar mode: Set marker 7 to current cursor position."
  (interactive)
  (setq ws-marker-7 (point-marker))
  (message "Marker 7 set"))

(defun ws-set-marker-8 ()
  "In WordStar mode: Set marker 8 to current cursor position."
  (interactive)
  (setq ws-marker-8 (point-marker))
  (message "Marker 8 set"))

(defun ws-set-marker-9 ()
  "In WordStar mode: Set marker 9 to current cursor position."
  (interactive)
  (setq ws-marker-9 (point-marker))
  (message "Marker 9 set"))

(defun ws-begin-block ()
  "In WordStar mode: Set block begin marker to current cursor position."
  (interactive)
  (setq ws-block-begin-marker (point-marker))
  (message "Block begin marker set"))

(defun ws-show-markers ()
  "In WordStar mode: Show block markers."
  (interactive)
  (if (or ws-block-begin-marker ws-block-end-marker)
      (save-excursion
	(if ws-block-begin-marker
	    (progn
	      (goto-char ws-block-begin-marker)
	      (message "Block begin marker")
	      (sit-for 2))
	  (message "Block begin marker not set")
	  (sit-for 2))
	(if ws-block-end-marker
	    (progn
	      (goto-char ws-block-end-marker)
	      (message "Block end marker")
	      (sit-for 2))
	  (message "Block end marker not set"))
	(message ""))
    (message "Block markers not set")))


(defun ws-indent-block ()
  "In WordStar mode: Indent block (not yet implemented)."
  (interactive)
  (ws-error "Indent block not yet implemented"))

(defun ws-end-block ()
  "In WordStar mode: Set block end marker to current cursor position."
  (interactive)
  (setq ws-block-end-marker (point-marker))
  (message "Block end marker set"))

(defun ws-print-block ()
  "In WordStar mode: Print block."
  (interactive)
  (message "Don't do this. Write block to a file (C-k w) and print this file."))

(defun ws-mark-word ()
  "In WordStar mode: Mark current word as block."
  (interactive)
  (save-excursion
    (forward-word 1)
    (sit-for 1)
    (ws-end-block)
    (forward-word -1)
    (sit-for 1)
    (ws-begin-block)))

(defun ws-exdent-block ()
  "I don't know what this (C-k u) should do."
  (interactive)
  (ws-error "This won't be done -- not yet implemented."))

(defun ws-move-block ()
  "In WordStar mode: Move block to current cursor position."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (progn
	(kill-region ws-block-begin-marker ws-block-end-marker)
	(yank)
	(save-excursion
	  (goto-char (region-beginning))
	  (setq ws-block-begin-marker (point-marker))
	  (goto-char (region-end))
	  (setq ws-block-end-marker (point-marker))))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
		    (ws-block-end-marker "Block begin marker not set")
		    (t "Block markers not set")))))

(defun ws-write-block ()
  "In WordStar mode: Write block to file."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (let ((filename (read-file-name "Write block to file: ")))
	(write-region ws-block-begin-marker ws-block-end-marker filename))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
		    (ws-block-end-marker "Block begin marker not set")
		    (t "Block markers not set")))))


(defun ws-delete-block ()
  "In WordStar mode: Delete block."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (progn
	(kill-region ws-block-begin-marker ws-block-end-marker)
	(setq ws-block-end-marker nil)
	(setq ws-block-begin-marker nil))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
		    (ws-block-end-marker "Block begin marker not set")
		    (t "Block markers not set")))))

(defun ws-find-marker-0 ()
  "In WordStar mode: Go to marker 0."
  (interactive)
  (if ws-marker-0
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-0))
    (ws-error "Marker 0 not set")))

(defun ws-find-marker-1 ()
  "In WordStar mode: Go to marker 1."
  (interactive)
  (if ws-marker-1
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-1))
    (ws-error "Marker 1 not set")))

(defun ws-find-marker-2 ()
  "In WordStar mode: Go to marker 2."
  (interactive)
  (if ws-marker-2
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-2))
    (ws-error "Marker 2 not set")))

(defun ws-find-marker-3 ()
  "In WordStar mode: Go to marker 3."
  (interactive)
  (if ws-marker-3
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-3))
    (ws-error "Marker 3 not set")))

(defun ws-find-marker-4 ()
  "In WordStar mode: Go to marker 4."
  (interactive)
  (if ws-marker-4
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-4))
    (ws-error "Marker 4 not set")))

(defun ws-find-marker-5 ()
  "In WordStar mode: Go to marker 5."
  (interactive)
  (if ws-marker-5
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-5))
    (ws-error "Marker 5 not set")))

(defun ws-find-marker-6 ()
  "In WordStar mode: Go to marker 6."
  (interactive)
  (if ws-marker-6
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-6))
    (ws-error "Marker 6 not set")))

(defun ws-find-marker-7 ()
  "In WordStar mode: Go to marker 7."
  (interactive)
  (if ws-marker-7
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-7))
    (ws-error "Marker 7 not set")))

(defun ws-find-marker-8 ()
  "In WordStar mode: Go to marker 8."
  (interactive)
  (if ws-marker-8
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-8))
    (ws-error "Marker 8 not set")))

(defun ws-find-marker-9 ()
  "In WordStar mode: Go to marker 9."
  (interactive)
  (if ws-marker-9
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-marker-9))
    (ws-error "Marker 9 not set")))

(defun ws-goto-block-begin ()
  "In WordStar mode: Go to block begin marker."
  (interactive)
  (if ws-block-begin-marker
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-block-begin-marker))
    (ws-error "Block begin marker not set")))

(defun ws-search (string)
  "In WordStar mode: Search string, remember string for repetition."
  (interactive "sSearch for: ")
  (message "Forward (f) or backward (b)")
  (let ((direction
	 (read-char)))
    (cond ((equal (upcase direction) ?F)
	   (setq ws-search-string string)
	   (setq ws-search-direction t)
	   (setq ws-last-cursorposition (point-marker))
	   (search-forward string))
	  ((equal (upcase direction) ?B)
	   (setq ws-search-string string)
	   (setq ws-search-direction nil)
	   (setq ws-last-cursorposition (point-marker))
	   (search-backward string))
	  (t (keyboard-quit)))))

(defun ws-goto-block-end ()
  "In WordStar mode: Go to block end marker."
  (interactive)
  (if ws-block-end-marker
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-block-end-marker))
    (ws-error "Block end marker not set")))

(defun ws-undo ()
  "In WordStar mode: Undo and give message about undoing more changes."
  (interactive)
  (undo)
  (message "Repeat C-q l to undo more changes."))

(defun ws-goto-last-cursorposition ()
  "In WordStar mode: "
  (interactive)
  (if ws-last-cursorposition
      (progn
	(setq ws-last-cursorposition (point-marker))
	(goto-char ws-last-cursorposition))
    (ws-error "No last cursor position available.")))

(defun ws-last-error ()
  "In WordStar mode: repeat last error message.
This will only work for errors raised by WordStar mode functions."
  (interactive)
  (if ws-last-errormessage
      (message "%s" ws-last-errormessage)
    (message "No WordStar error yet.")))

(defun ws-kill-eol ()
  "In WordStar mode: Kill to end of line (like WordStar, not like Emacs)."
  (interactive)
  (let ((p (point)))
    (end-of-line)
    (kill-region p (point))))

(defun ws-kill-bol ()
  "In WordStar mode: Kill to beginning of line
\(like WordStar, not like Emacs)."
  (interactive)
  (let ((p (point)))
    (beginning-of-line)
    (kill-region (point) p)))

(defun kill-complete-line ()
  "Kill the complete line."
  (interactive)
  (beginning-of-line)
  (if (eobp) (error "End of buffer"))
  (let ((beg (point)))
    (forward-line 1)
    (kill-region beg (point))))

(defun ws-repeat-search ()
  "In WordStar mode: Repeat last search."
  (interactive)
  (setq ws-last-cursorposition (point-marker))
  (if ws-search-string
      (if ws-search-direction
	  (search-forward ws-search-string)
	(search-backward ws-search-string))
    (ws-error "No search to repeat")))

(defun ws-query-replace (from to)
  "In WordStar mode: Search string, remember string for repetition."
  (interactive "sReplace: \n\
sWith: " )
  (setq ws-search-string from)
  (setq ws-search-direction t)
  (setq ws-last-cursorposition (point-marker))
  (query-replace from to))

(defun ws-copy-block ()
  "In WordStar mode: Copy block to current cursor position."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (progn
	(copy-region-as-kill ws-block-begin-marker ws-block-end-marker)
	(yank)
	(save-excursion
	  (goto-char (region-beginning))
	  (setq ws-block-begin-marker (point-marker))
	  (goto-char (region-end))
	  (setq ws-block-end-marker (point-marker))))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
		    (ws-block-end-marker "Block begin marker not set")
		    (t "Block markers not set")))))

(provide 'ws-mode)

;;; ws-mode.el ends here
