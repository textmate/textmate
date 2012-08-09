;;; refill.el --- `auto-fill' by refilling paragraphs on changes

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Maintainer: Miles Bader <miles@gnu.org>
;; Keywords: wp

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

;; Provides a mode where paragraphs are refilled after changes in them
;; (using `after-change-functions').  This gives something akin to typical
;; word processor-style filling.  We restrict refilling due to
;; self-insertion to the characters which trigger auto-fill.

;; It partly satisfies a todo item in enriched.el for some value of
;; `without slowing down editing too much'.  It doesn't attempt to do
;; anything (using `window-size-change-functions'?) about resizing
;; windows -- who cares?

;; This implementation is probably fragile and missing some special
;; cases -- not extensively tested.  Yanking paragraph breaks, for
;; instance, won't DTRT by refilling all the relevant paragraphs.

;; You could do it a bit more efficiently (and robustly?) with just an
;; auto-fill function, but that doesn't cope with changes other than
;; through self-insertion.  (Using auto-fill and after-change
;; functions together didn't seem winning.)  This could probably
;; benefit from a less-general and faster `fill-paragraph-function',
;; ideally as a primitive.

;; The work is done in a local post-command hook but only if
;; `refill-doit' has been set by the after-change function.  Using
;; `post-command-hook' ensures simply that refilling only happens
;; once per command.

;; [Per Abrahamsen's maniac.el does a similar thing, but operates from
;; post-command-hook.  I don't understand the statement in it that
;; after-change-functions don't work for this purpose; perhaps there was
;; some Emacs bug at the time.  ISTR maniac has problems with
;; whitespace at the end of paragraphs.]

;;; Todo/Bugs:

;; - When deleting the first word on a line, the space after that word tends
;;   to become part of the fill-prefix, causing either wrong filling of the
;;   remaining text, or causing the cursor to move unexpectedly.  Ex:
;;   Start with
;;                I>< blabla
;;
;;   and hit backspace.  We end up with
;;
;;                 ><blabla
;;   instead of
;;                >< blabla
;;
;;   Other example.  Start with
;;
;;     Foo bar blablabla asdgf
;;     word>< asdfas dfasdfasd
;;     asd asdfa sdfasd sdf
;;
;;   and hit M-backspace.  We end up with
;;
;;     Foo bar blablabla asdgf
;;      ><asdfas dfasdfasd asd
;;      asdfa sdfasd sdf

;;; Code:

(eval-when-compile (require 'cl))

(defgroup refill nil
  "Refilling paragraphs on changes."
  :group 'fill)

(defvar refill-ignorable-overlay nil
  "Portion of the most recently filled paragraph not needing filling.
This is used to optimize refilling.")
(make-variable-buffer-local 'refill-ignorable-overlay)

(defun refill-adjust-ignorable-overlay (overlay afterp beg end &optional len)
  "Adjust OVERLAY to not include the about-to-be-modified region."
  (when (not afterp)
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (if (<= (point) (overlay-start overlay))
	  ;; Just get OVERLAY out of the way
	  (move-overlay overlay (point-min) (point-min))
	;; Make overlay contain only the region
	(move-overlay overlay (overlay-start overlay) (point))))))

(defun refill-fill-paragraph-at (pos &optional arg)
  "Like `fill-paragraph' at POS, but don't delete whitespace at paragraph end."
  (save-excursion
    (goto-char pos)
    ;; FIXME: forward-paragraph seems to disregard `use-hard-newlines',
    ;; leading to excessive refilling and wrong choice of fill-prefix.
    ;; might be a bug in my paragraphs.el.
    (forward-paragraph)
    (skip-syntax-backward "-")
    (let ((end (point))
	  (beg (progn (backward-paragraph) (point)))
	  (obeg (overlay-start refill-ignorable-overlay))
	  (oend (overlay-end refill-ignorable-overlay)))
      (unless (> beg pos)      ;Don't fill if point is outside the paragraph.
	(goto-char pos)
	(if (and (>= beg obeg) (< beg oend))
	    ;; Limit filling to the modified tail of the paragraph.
	    (let ( ;; When adaptive-fill-mode is enabled, the filling
		  ;; functions will attempt to set the fill prefix from
		  ;; the fake paragraph bounds we pass in, so set it
		  ;; ourselves first, using the real paragraph bounds.
		  (fill-prefix
		   (if (and adaptive-fill-mode
			    (or (null fill-prefix) (string= fill-prefix "")))
		       (fill-context-prefix beg end)
		     fill-prefix))
		  ;; Turn off adaptive-fill-mode temporarily
		  (adaptive-fill-mode nil))
	      (save-restriction
		(if use-hard-newlines
		    (fill-region oend end arg)
		  (fill-region-as-paragraph oend end arg)))
	      (move-overlay refill-ignorable-overlay obeg (point)))
	  ;; Fill the whole paragraph
	  (save-restriction
	    (if use-hard-newlines
		(fill-region beg end arg)
	      (fill-region-as-paragraph beg end arg)))
	  (move-overlay refill-ignorable-overlay beg (point)))))))

(defun refill-fill-paragraph (arg)
  "Like `fill-paragraph' but don't delete whitespace at paragraph end."
  (refill-fill-paragraph-at (point) arg))

(defvar refill-doit nil
  "Non-nil tells `refill-post-command-function' to do its processing.
Set by `refill-after-change-function' in `after-change-functions' and
unset by `refill-post-command-function' in `post-command-hook', and
sometimes `refill-pre-command-function' in `pre-command-hook'.  This
ensures refilling is only done once per command that causes a change,
regardless of the number of after-change calls from commands doing
complex processing.")
(make-variable-buffer-local 'refill-doit)

(defun refill-after-change-function (beg end len)
  "Function for `after-change-functions' which just sets `refill-doit'."
  (unless undo-in-progress
    (setq refill-doit end)))

(defun refill-post-command-function ()
  "Post-command function to do refilling (conditionally)."
  (when refill-doit ; there was a change
    ;; There's probably scope for more special cases here...
    (case this-command
      (self-insert-command
       ;; Treat self-insertion commands specially, since they don't
       ;; always reset `refill-doit' -- for self-insertion commands that
       ;; *don't* cause a refill, we want to leave it turned on so that
       ;; any subsequent non-modification command will cause a refill.
       (when (aref auto-fill-chars (char-before))
	 ;; Respond to the same characters as auto-fill (other than
	 ;; newline, covered below).
	 (refill-fill-paragraph-at refill-doit)
	 (setq refill-doit nil)))
      ((quoted-insert fill-paragraph fill-region) nil)
      ((newline newline-and-indent open-line indent-new-comment-line
	reindent-then-newline-and-indent)
       ;; Don't zap what was just inserted.
       (save-excursion
	 (beginning-of-line)		; for newline-and-indent
	 (skip-chars-backward "\n")
	 (save-restriction
	   (narrow-to-region (point-min) (point))
	   (refill-fill-paragraph-at refill-doit)))
       (widen)
       (save-excursion
	 (skip-chars-forward "\n")
	 (save-restriction
	   (narrow-to-region (line-beginning-position) (point-max))
	   (refill-fill-paragraph-at refill-doit))))
      (t
       (refill-fill-paragraph-at refill-doit)))
    (setq refill-doit nil)))

(defun refill-pre-command-function ()
  "Pre-command function to do refilling (conditionally)."
  (when (and refill-doit (not (eq this-command 'self-insert-command)))
    ;; A previous setting of `refill-doit' didn't result in a refill,
    ;; because it was a self-insert-command.  Since the next command is
    ;; something else, do the refill now.
    (refill-fill-paragraph-at refill-doit)
    (setq refill-doit nil)))

(defvar refill-saved-state nil)

;;;###autoload
(define-minor-mode refill-mode
  "Toggle automatic refilling (Refill mode).
With a prefix argument ARG, enable Refill mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Refill mode is a buffer-local minor mode.  When enabled, the
current paragraph is refilled as you edit.  Self-inserting
characters only cause refilling if they would cause
auto-filling.

For true \"word wrap\" behavior, use `visual-line-mode' instead."
  :group 'refill
  :lighter " Refill"
  :keymap '(("\177" . backward-delete-char-untabify))
  ;; Remove old state if necessary
  (when refill-ignorable-overlay
    (delete-overlay refill-ignorable-overlay)
    (kill-local-variable 'refill-ignorable-overlay))
  (when (local-variable-p 'refill-saved-state)
    (dolist (x refill-saved-state)
      (set (make-local-variable (car x)) (cdr x)))
    (kill-local-variable 'refill-saved-state))
  (if refill-mode
      (progn
	(add-hook 'after-change-functions 'refill-after-change-function nil t)
	(add-hook 'post-command-hook 'refill-post-command-function nil t)
	(add-hook 'pre-command-hook 'refill-pre-command-function nil t)
	(set (make-local-variable 'refill-saved-state)
	     (mapcar (lambda (s) (cons s (symbol-value s)))
		     '(fill-paragraph-function auto-fill-function)))
	;; This provides the test for recursive paragraph filling.
	(set (make-local-variable 'fill-paragraph-function)
	     'refill-fill-paragraph)
	;; When using justification, doing DEL on 2 spaces should remove
	;; both, otherwise, the subsequent refill will undo the DEL.
	(set (make-local-variable 'backward-delete-char-untabify-method)
	     'hungry)
	(setq refill-ignorable-overlay (make-overlay 1 1 nil nil t))
	(overlay-put refill-ignorable-overlay 'modification-hooks
		     '(refill-adjust-ignorable-overlay))
	(overlay-put refill-ignorable-overlay 'insert-behind-hooks
		     '(refill-adjust-ignorable-overlay))
	(auto-fill-mode 0))
    (remove-hook 'after-change-functions 'refill-after-change-function t)
    (remove-hook 'post-command-hook 'refill-post-command-function t)
    (kill-local-variable 'backward-delete-char-untabify-method)))

(provide 'refill)

;;; refill.el ends here
