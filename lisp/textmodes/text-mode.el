;;; text-mode.el --- text mode, and its idiosyncratic commands

;; Copyright (C) 1985, 1992, 1994, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp
;; Package: emacs

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

;; This package provides the fundamental text mode documented in the
;; Emacs user's manual.

;;; Code:

(defcustom text-mode-hook nil
  "Normal hook run when entering Text mode and many related modes."
  :type 'hook
  :options '(turn-on-auto-fill turn-on-flyspell)
  :group 'wp)

(defvar text-mode-variant nil
  "Non-nil if this buffer's major mode is a variant of Text mode.
Use (derived-mode-p 'text-mode) instead.")

(defvar text-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    ;; We add `p' so that M-c on 'hello' leads to 'Hello' rather than 'hello'.
    (modify-syntax-entry ?' "w p" st)
    st)
  "Syntax table used while in `text-mode'.")

(defvar text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\t" 'ispell-complete-word)
    map)
  "Keymap for `text-mode'.
Many other modes, such as `mail-mode', `outline-mode' and `indented-text-mode',
inherit all the commands defined in this map.")


(define-derived-mode text-mode nil "Text"
  "Major mode for editing text written for humans to read.
In this mode, paragraphs are delimited only by blank or white lines.
You can thus get the full benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{text-mode-map}
Turning on Text mode runs the normal hook `text-mode-hook'."
  (set (make-local-variable 'text-mode-variant) t)
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (set (make-local-variable 'indent-line-function) 'indent-relative))

(define-derived-mode paragraph-indent-text-mode text-mode "Parindent"
  "Major mode for editing text, with leading spaces starting a paragraph.
In this mode, you do not need blank lines between paragraphs
when the first line of the following paragraph starts with whitespace.
`paragraph-indent-minor-mode' provides a similar facility as a minor mode.
Special commands:
\\{text-mode-map}
Turning on Paragraph-Indent Text mode runs the normal hooks
`text-mode-hook' and `paragraph-indent-text-mode-hook'."
  :abbrev-table nil :syntax-table nil
  (paragraph-indent-minor-mode))

(defun paragraph-indent-minor-mode ()
  "Minor mode for editing text, with leading spaces starting a paragraph.
In this mode, you do not need blank lines between paragraphs when the
first line of the following paragraph starts with whitespace, as with
`paragraph-indent-text-mode'.
Turning on Paragraph-Indent minor mode runs the normal hook
`paragraph-indent-text-mode-hook'."
  (interactive)
  (set (make-local-variable 'paragraph-start)
       (concat "[ \t\n\f]\\|" paragraph-start))
  (set (make-local-variable 'indent-line-function) 'indent-to-left-margin)
  (run-hooks 'paragraph-indent-text-mode-hook))

(defalias 'indented-text-mode 'text-mode)

;; This can be made a no-op once all modes that use text-mode-hook
;; are "derived" from text-mode.
(defun text-mode-hook-identify ()
  "Mark that this mode has run `text-mode-hook'.
This is how `toggle-text-mode-auto-fill' knows which buffers to operate on."
  (set (make-local-variable 'text-mode-variant) t))

(add-hook 'text-mode-hook 'text-mode-hook-identify)

(defun toggle-text-mode-auto-fill ()
  "Toggle whether to use Auto Fill in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (interactive)
  (let ((enable-mode (not (memq 'turn-on-auto-fill text-mode-hook))))
    (if enable-mode
	(add-hook 'text-mode-hook 'turn-on-auto-fill)
      (remove-hook 'text-mode-hook 'turn-on-auto-fill))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (or (derived-mode-p 'text-mode) text-mode-variant)
	    (auto-fill-mode (if enable-mode 1 0)))))
    (message "Auto Fill %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))


(define-key facemenu-keymap "\eS" 'center-paragraph)

(defun center-paragraph ()
  "Center each nonblank line in the paragraph at or after point.
See `center-line' for more info."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (center-region (point) end))))

(defun center-region (from to)
  "Center each nonblank line starting in the region.
See `center-line' for more info."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(or (save-excursion (skip-chars-forward " \t") (eolp))
	    (center-line))
	(forward-line 1)))))

(define-key facemenu-keymap "\es" 'center-line)

(defun center-line (&optional nlines)
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation so that it equals
the distance between the end of the text and `fill-column'.
The argument NLINES says how many lines to center."
  (interactive "P")
  (if nlines (setq nlines (prefix-numeric-value nlines)))
  (while (not (eq nlines 0))
    (save-excursion
      (let ((lm (current-left-margin))
	    line-length)
	(beginning-of-line)
	(delete-horizontal-space)
	(end-of-line)
	(delete-horizontal-space)
	(setq line-length (current-column))
	(if (> (- fill-column lm line-length) 0)
	    (indent-line-to
	     (+ lm (/ (- fill-column lm line-length) 2))))))
    (cond ((null nlines)
	   (setq nlines 0))
	  ((> nlines 0)
	   (setq nlines (1- nlines))
	   (forward-line 1))
	  ((< nlines 0)
	   (setq nlines (1+ nlines))
	   (forward-line -1)))))

;;; text-mode.el ends here
