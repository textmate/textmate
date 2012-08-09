;;; em-prompt.el --- command prompts

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Most of the prompt navigation commands of `comint-mode' are
;; supported, such as C-c C-n, C-c C-p, etc.

;;; Code:

(eval-when-compile (require 'eshell))

;;;###autoload
(eshell-defgroup eshell-prompt nil
  "This module provides command prompts, and navigation between them,
as is common with most shells."
  :tag "Command prompts"
  :group 'eshell-module)

;;; User Variables:

(defcustom eshell-prompt-load-hook nil
  "A list of functions to call when loading `eshell-prompt'."
  :version "24.1"			; removed eshell-prompt-initialize
  :type 'hook
  :group 'eshell-prompt)

(defcustom eshell-prompt-function
  (function
   (lambda ()
     (concat (abbreviate-file-name (eshell/pwd))
	     (if (= (user-uid) 0) " # " " $ "))))
  "A function that returns the Eshell prompt string.
Make sure to update `eshell-prompt-regexp' so that it will match your
prompt."
  :type 'function
  :group 'eshell-prompt)

(defcustom eshell-prompt-regexp "^[^#$\n]* [#$] "
  "A regexp which fully matches your eshell prompt.
This setting is important, since it affects how eshell will interpret
the lines that are passed to it.
If this variable is changed, all Eshell buffers must be exited and
re-entered for it to take effect."
  :type 'regexp
  :group 'eshell-prompt)

(defcustom eshell-highlight-prompt t
  "If non-nil, Eshell should highlight the prompt."
  :type 'boolean
  :group 'eshell-prompt)

(defface eshell-prompt
  '((((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:bold t)))
  "The face used to highlight prompt strings.
For highlighting other kinds of strings -- similar to shell mode's
behavior -- simply use an output filer which changes text properties."
  :group 'eshell-prompt)
(define-obsolete-face-alias 'eshell-prompt-face 'eshell-prompt "22.1")

(defcustom eshell-before-prompt-hook nil
  "A list of functions to call before outputting the prompt."
  :type 'hook
  :options '(eshell-begin-on-new-line)
  :group 'eshell-prompt)

(defcustom eshell-after-prompt-hook nil
  "A list of functions to call after outputting the prompt.
Note that if `eshell-scroll-show-maximum-output' is non-nil, then
setting `eshell-show-maximum-output' here won't do much.  It depends
on whether the user wants the resizing to happen while output is
arriving, or after."
  :type 'hook
  :options '(eshell-show-maximum-output)
  :group 'eshell-prompt)

;;; Functions:

(defun eshell-prompt-initialize ()
  "Initialize the prompting code."
  (unless eshell-non-interactive-p
    (add-hook 'eshell-post-command-hook 'eshell-emit-prompt nil t)

    (make-local-variable 'eshell-prompt-regexp)
    (if eshell-prompt-regexp
	(set (make-local-variable 'paragraph-start) eshell-prompt-regexp))

    (set (make-local-variable 'eshell-skip-prompt-function)
	 'eshell-skip-prompt)

    (define-key eshell-command-map [(control ?n)] 'eshell-next-prompt)
    (define-key eshell-command-map [(control ?p)] 'eshell-previous-prompt)))

(defun eshell-emit-prompt ()
  "Emit a prompt if eshell is being used interactively."
  (run-hooks 'eshell-before-prompt-hook)
  (if (not eshell-prompt-function)
      (set-marker eshell-last-output-end (point))
    (let ((prompt (funcall eshell-prompt-function)))
      (and eshell-highlight-prompt
	   (add-text-properties 0 (length prompt)
				'(read-only t
				  face eshell-prompt
				  rear-nonsticky (face read-only))
				prompt))
      (eshell-interactive-print prompt)))
  (run-hooks 'eshell-after-prompt-hook))

(defun eshell-backward-matching-input (regexp arg)
  "Search backward through buffer for match for REGEXP.
Matches are searched for on lines that match `eshell-prompt-regexp'.
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (eshell-regexp-arg "Backward input matching (regexp): "))
  (let* ((re (concat eshell-prompt-regexp ".*" regexp))
	 (pos (save-excursion (end-of-line (if (> arg 0) 0 1))
			      (if (re-search-backward re nil t arg)
				  (point)))))
    (if (null pos)
	(progn (message "Not found")
	       (ding))
      (goto-char pos)
      (eshell-bol))))

(defun eshell-forward-matching-input (regexp arg)
  "Search forward through buffer for match for REGEXP.
Matches are searched for on lines that match `eshell-prompt-regexp'.
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (eshell-regexp-arg "Forward input matching (regexp): "))
  (eshell-backward-matching-input regexp (- arg)))

(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
  (interactive "p")
  (forward-paragraph n)
  (eshell-skip-prompt))

(defun eshell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
  (interactive "p")
  (eshell-next-prompt (- (1+ n))))

(defun eshell-skip-prompt ()
  "Skip past the text matching regexp `eshell-prompt-regexp'.
If this takes us past the end of the current line, don't skip at all."
  (let ((eol (line-end-position)))
    (if (and (looking-at eshell-prompt-regexp)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))

(provide 'em-prompt)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-prompt.el ends here
