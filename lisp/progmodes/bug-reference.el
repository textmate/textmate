;; bug-reference.el --- buttonize bug references

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 21 Mar 2007
;; Keywords: tools

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

;; This file provides minor modes for putting clickable overlays on
;; references to bugs.  A bug reference is text like "PR foo/29292";
;; this is mapped to a URL using a user-supplied format.

;; Two minor modes are provided.  One works on any text in the buffer;
;; the other operates only on comments and strings.

(defvar bug-reference-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'bug-reference-push-button)
    (define-key map (kbd "C-c RET") 'bug-reference-push-button)
    map)
  "Keymap used by bug reference buttons.")

;; E.g., "http://gcc.gnu.org/PR%s"
(defvar bug-reference-url-format nil
  "Format used to turn a bug number into a URL.
The bug number is supplied as a string, so this should have a single %s.
This can also be a function designator; it is called without arguments
 and should return a string.
It can use `match-string' to get parts matched against
`bug-reference-bug-regexp', specifically:
 1. issue kind (bug, patch, rfe &c)
 2. issue number.

There is no default setting for this, it must be set per file.
If you set it to a symbol in the file Local Variables section,
you need to add a `bug-reference-url-format' property to it:
\(put 'my-bug-reference-url-format 'bug-reference-url-format t)
so that it is considered safe, see `enable-local-variables'.")

;;;###autoload
(put 'bug-reference-url-format 'safe-local-variable
     (lambda (s)
       (or (stringp s)
           (and (symbolp s)
                (get s 'bug-reference-url-format)))))

(defconst bug-reference-bug-regexp
  "\\([Bb]ug ?#\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z-+]+/\\)\\([0-9]+\\)"
  "Regular expression which matches bug references.")

(defun bug-reference-set-overlay-properties ()
  "Set properties of bug reference overlays."
  (put 'bug-reference 'evaporate t)
  (put 'bug-reference 'face 'link)
  (put 'bug-reference 'mouse-face 'highlight)
  (put 'bug-reference 'help-echo "mouse-1, C-c RET: visit this bug")
  (put 'bug-reference 'keymap bug-reference-map)
  (put 'bug-reference 'follow-link t))

(bug-reference-set-overlay-properties)

(defun bug-reference-unfontify (start end)
  "Remove bug reference overlays from region."
  (dolist (o (overlays-in start end))
    (when (eq (overlay-get o 'category) 'bug-reference)
      (delete-overlay o))))

(defvar bug-reference-prog-mode)

(defun bug-reference-fontify (start end)
  "Apply bug reference overlays to region."
  (save-excursion
    (let ((beg-line (progn (goto-char start) (line-beginning-position)))
	  (end-line (progn (goto-char end) (line-end-position))))
      ;; Remove old overlays.
      (bug-reference-unfontify beg-line end-line)
      (goto-char beg-line)
      (while (and (< (point) end-line)
		  (re-search-forward bug-reference-bug-regexp end-line 'move))
	(when (or (not bug-reference-prog-mode)
		  ;; This tests for both comment and string syntax.
		  (nth 8 (syntax-ppss)))
	  (let ((overlay (make-overlay (match-beginning 0) (match-end 0)
				       nil t nil)))
	    (overlay-put overlay 'category 'bug-reference)
	    ;; Don't put a link if format is undefined
	    (when bug-reference-url-format
              (overlay-put overlay 'bug-reference-url
                           (if (stringp bug-reference-url-format)
                               (format bug-reference-url-format
                                       (match-string-no-properties 2))
                             (funcall bug-reference-url-format))))))))))

;; Taken from button.el.
(defun bug-reference-push-button (&optional pos _use-mouse-action)
  "Open URL corresponding to the bug reference at POS."
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)))
  (if (and (not (integerp pos)) (eventp pos))
      ;; POS is a mouse event; switch to the proper window/buffer
      (let ((posn (event-start pos)))
	(with-current-buffer (window-buffer (posn-window posn))
	  (bug-reference-push-button (posn-point posn) t)))
    ;; POS is just normal position.
    (dolist (o (overlays-at pos))
      ;; It should only be possible to have one URL overlay.
      (let ((url (overlay-get o 'bug-reference-url)))
	(when url
	  (browse-url url))))))

;;;###autoload
(define-minor-mode bug-reference-mode
  "Toggle hyperlinking bug references in the buffer (Bug Reference mode).
With a prefix argument ARG, enable Bug Reference mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  nil
  ""
  nil
  (if bug-reference-mode
      (jit-lock-register #'bug-reference-fontify)
    (jit-lock-unregister #'bug-reference-fontify)
    (save-restriction
      (widen)
      (bug-reference-unfontify (point-min) (point-max)))))

;;;###autoload
(define-minor-mode bug-reference-prog-mode
  "Like `bug-reference-mode', but only buttonize in comments and strings."
  nil
  ""
  nil
  (if bug-reference-prog-mode
      (jit-lock-register #'bug-reference-fontify)
    (jit-lock-unregister #'bug-reference-fontify)
    (save-restriction
      (widen)
      (bug-reference-unfontify (point-min) (point-max)))))

;;; bug-reference.el ends here
