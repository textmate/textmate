;;; minibuf-eldef.el --- Only show defaults in prompts when applicable
;;
;; Copyright (C) 2000-2012 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
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
;;
;; Defines the mode `minibuffer-electric-default-mode'.
;;
;; When active, minibuffer prompts that show a default value only show
;; the default when it's applicable -- that is, when hitting RET would
;; yield the default value.  If the user modifies the input such that
;; hitting RET would enter a non-default value, the prompt is modified
;; to remove the default indication (which is restored if the input is
;; ever restore to the match the initial input).

;;; Code:

(defvar minibuffer-default-in-prompt-regexps
  '(("\\( (default\\>.*)\\):? \\'" . 1) ("\\( \\[.*\\]\\):? *\\'" . 1))
  "*A list of regexps matching the parts of minibuffer prompts showing defaults.
When `minibuffer-electric-default-mode' is active, these regexps are
used to identify the portions of prompts to elide.

Each entry is either a string, which should be a regexp matching the
default portion of the prompt, or a cons cell, who's car is a regexp
matching the default part of the prompt, and who's cdr indicates the
regexp subexpression that matched.")


;;; Internal variables

;; A list of minibuffers to which we've added a post-command-hook.
(defvar minibuf-eldef-frobbed-minibufs nil)

;;; The following are all local variables in the minibuffer

;; Input pre-inserted into the minibuffer before the user can edit it.
(defvar minibuf-eldef-initial-input)
(make-variable-buffer-local 'minibuf-eldef-initial-input)
;; and the length of the buffer with it inserted.
(defvar minibuf-eldef-initial-buffer-length)
(make-variable-buffer-local 'minibuf-eldef-initial-buffer-length)

;; True if the current minibuffer prompt contains the default spec.
(defvar minibuf-eldef-showing-default-in-prompt)
(make-variable-buffer-local 'minibuf-eldef-showing-default-in-prompt)

;; An overlay covering the default portion of the prompt
(defvar minibuf-eldef-overlay)
(make-variable-buffer-local 'minibuf-eldef-overlay)


;;; Hook functions

;; This function goes on minibuffer-setup-hook
(defun minibuf-eldef-setup-minibuffer ()
  "Set up a minibuffer for `minibuffer-electric-default-mode'.
The prompt and initial input should already have been inserted."
  (let ((regexps minibuffer-default-in-prompt-regexps)
	(match nil)
	(inhibit-point-motion-hooks t))
    (save-excursion
      (save-restriction
	;; Narrow to only the prompt
	(goto-char (point-min))
	(narrow-to-region (point) (minibuffer-prompt-end))
	;; See the prompt contains a default input indicator
	(while regexps
	  (setq match (pop regexps))
	  (if (re-search-forward (if (stringp match) match (car match)) nil t)
	      (setq regexps nil)
	    (setq match nil)))))
    (if (not match)
	;; Nope, so just make sure our post-command-hook isn't left around.
	(remove-hook 'post-command-hook #'minibuf-eldef-update-minibuffer t)
      ;; Yup; set things up so we can frob the prompt as the state of
      ;; the input string changes.
      (setq match (if (consp match) (cdr match) 0))
      (setq minibuf-eldef-overlay
	    (make-overlay (match-beginning match) (match-end match)))
      (setq minibuf-eldef-showing-default-in-prompt t)
      (setq minibuf-eldef-initial-input
	    (minibuffer-contents-no-properties))
      (setq minibuf-eldef-initial-buffer-length (point-max))
      (add-to-list 'minibuf-eldef-frobbed-minibufs (current-buffer))
      (add-hook 'post-command-hook #'minibuf-eldef-update-minibuffer nil t))))

;; post-command-hook to swap prompts when necessary
(defun minibuf-eldef-update-minibuffer ()
  "Update a minibuffer's prompt to include a default only when applicable.
This is intended to be used as a minibuffer post-command-hook for
`minibuffer-electric-default-mode'; the minibuffer should have already
been set up by `minibuf-eldef-setup-minibuffer'."
  (unless (eq minibuf-eldef-showing-default-in-prompt
	      (and (= (point-max) minibuf-eldef-initial-buffer-length)
		   (string-equal (minibuffer-contents-no-properties)
				 minibuf-eldef-initial-input)))
    ;; swap state
    (setq minibuf-eldef-showing-default-in-prompt
	  (not minibuf-eldef-showing-default-in-prompt))
    (cond (minibuf-eldef-showing-default-in-prompt
	   (overlay-put minibuf-eldef-overlay 'invisible nil)
	   (overlay-put minibuf-eldef-overlay 'intangible nil))
	  (t
	   (overlay-put minibuf-eldef-overlay 'invisible t)
	   (overlay-put minibuf-eldef-overlay 'intangible t)))))


;;; Note this definition must be at the end of the file, because
;;; `define-minor-mode' actually calls the mode-function if the
;;; associated variable is non-nil, which requires that all needed
;;; functions be already defined.  [This is arguably a bug in d-m-m]
;;;###autoload
(define-minor-mode minibuffer-electric-default-mode
  "Toggle Minibuffer Electric Default mode.
With a prefix argument ARG, enable Minibuffer Electric Default
mode if ARG is positive, and disable it otherwise.  If called
from Lisp, enable the mode if ARG is omitted or nil.

Minibuffer Electric Default mode is a global minor mode.  When
enabled, minibuffer prompts that show a default value only show
the default when it's applicable -- that is, when hitting RET
would yield the default value.  If the user modifies the input
such that hitting RET would enter a non-default value, the prompt
is modified to remove the default indication."
  :global t
  :group 'minibuffer
  (if minibuffer-electric-default-mode
      ;; Enable the mode
      (add-hook 'minibuffer-setup-hook 'minibuf-eldef-setup-minibuffer)
    ;; Disable the mode
    (remove-hook 'minibuffer-setup-hook 'minibuf-eldef-setup-minibuffer)
    ;; Remove our entry from any post-command-hook variable's it's still in
    (dolist (minibuf minibuf-eldef-frobbed-minibufs)
      (with-current-buffer minibuf
	(remove-hook 'post-command-hook #'minibuf-eldef-update-minibuffer t)))
    (setq minibuf-eldef-frobbed-minibufs nil)))


(provide 'minibuf-eldef)

;;; minibuf-eldef.el ends here
