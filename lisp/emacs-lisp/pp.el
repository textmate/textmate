;;; pp.el --- pretty printer for Emacs Lisp

;; Copyright (C) 1989, 1993, 2001-2012 Free Software Foundation, Inc.

;; Author: Randal Schwartz <merlyn@stonehenge.com>
;; Keywords: lisp

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

(defvar font-lock-verbose)

(defgroup pp nil
  "Pretty printer for Emacs Lisp."
  :prefix "pp-"
  :group 'lisp)

(defcustom pp-escape-newlines t
  "Value of `print-escape-newlines' used by pp-* functions."
  :type 'boolean
  :group 'pp)

;;;###autoload
(defun pp-to-string (object)
  "Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ((print-escape-newlines pp-escape-newlines)
          (print-quoted t))
      (prin1 object (current-buffer)))
    (pp-buffer)
    (buffer-string)))

;;;###autoload
(defun pp-buffer ()
  "Prettify the current buffer with printed representation of a Lisp object."
  (goto-char (point-min))
  (while (not (eobp))
    ;; (message "%06d" (- (point-max) (point)))
    (cond
     ((ignore-errors (down-list 1) t)
      (save-excursion
        (backward-char 1)
        (skip-chars-backward "'`#^")
        (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
          (delete-region
           (point)
           (progn (skip-chars-backward " \t\n") (point)))
          (insert "\n"))))
     ((ignore-errors (up-list 1) t)
      (while (looking-at-p "\\s)")
        (forward-char 1))
      (delete-region
       (point)
       (progn (skip-chars-forward " \t\n") (point)))
      (insert ?\n))
     (t (goto-char (point-max)))))
  (goto-char (point-min))
  (indent-sexp))

;;;###autoload
(defun pp (object &optional stream)
  "Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)."
  (princ (pp-to-string object) (or stream standard-output)))

(defun pp-display-expression (expression out-buffer-name)
  "Prettify and display EXPRESSION in an appropriate way, depending on length.
If a temporary buffer is needed for representation, it will be named
after OUT-BUFFER-NAME."
  (let* ((old-show-function temp-buffer-show-function)
	 ;; Use this function to display the buffer.
	 ;; This function either decides not to display it at all
	 ;; or displays it in the usual way.
	 (temp-buffer-show-function
	  (function
	   (lambda (buf)
	     (with-current-buffer buf
	       (goto-char (point-min))
	       (end-of-line 1)
	       (if (or (< (1+ (point)) (point-max))
		       (>= (- (point) (point-min)) (frame-width)))
		   (let ((temp-buffer-show-function old-show-function)
			 (old-selected (selected-window))
			 (window (display-buffer buf)))
		     (goto-char (point-min)) ; expected by some hooks ...
		     (make-frame-visible (window-frame window))
		     (unwind-protect
			 (progn
			   (select-window window)
			   (run-hooks 'temp-buffer-show-hook))
		       (select-window old-selected)
		       (message "See buffer %s." out-buffer-name)))
		 (message "%s" (buffer-substring (point-min) (point)))
		 ))))))
    (with-output-to-temp-buffer out-buffer-name
      (pp expression)
      (with-current-buffer standard-output
	(emacs-lisp-mode)
	(setq buffer-read-only nil)
	(set (make-local-variable 'font-lock-verbose) nil)))))

;;;###autoload
(defun pp-eval-expression (expression)
  "Evaluate EXPRESSION and pretty-print its value.
Also add the value to the front of the list in the variable `values'."
  (interactive
   (list (read-from-minibuffer "Eval: " nil read-expression-map t
			       'read-expression-history)))
  (message "Evaluating...")
  (setq values (cons (eval expression) values))
  (pp-display-expression (car values) "*Pp Eval Output*"))

;;;###autoload
(defun pp-macroexpand-expression (expression)
  "Macroexpand EXPRESSION and pretty-print its value."
  (interactive
   (list (read-from-minibuffer "Macroexpand: " nil read-expression-map t
			       'read-expression-history)))
  (pp-display-expression (macroexpand expression) "*Pp Macroexpand Output*"))

(defun pp-last-sexp ()
  "Read sexp before point.  Ignores leading comment characters."
  (let ((stab (syntax-table)) (pt (point)) start exp)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (save-excursion
      (forward-sexp -1)
      ;; If first line is commented, ignore all leading comments:
      (if (save-excursion (beginning-of-line) (looking-at-p "[ \t]*;"))
	  (progn
	    (setq exp (buffer-substring (point) pt))
	    (while (string-match "\n[ \t]*;+" exp start)
	      (setq start (1+ (match-beginning 0))
		    exp (concat (substring exp 0 start)
				(substring exp (match-end 0)))))
	    (setq exp (read exp)))
	(setq exp (read (current-buffer)))))
    (set-syntax-table stab)
    exp))

;;;###autoload
(defun pp-eval-last-sexp (arg)
  "Run `pp-eval-expression' on sexp before point.
With argument, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (eval (pp-last-sexp))))
    (pp-eval-expression (pp-last-sexp))))

;;;###autoload
(defun pp-macroexpand-last-sexp (arg)
  "Run `pp-macroexpand-expression' on sexp before point.
With argument, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (macroexpand (pp-last-sexp))))
    (pp-macroexpand-expression (pp-last-sexp))))

;;; Test cases for quote
;; (pp-eval-expression ''(quote quote))
;; (pp-eval-expression ''((quote a) (quote b)))
;; (pp-eval-expression ''('a 'b))	; same as above
;; (pp-eval-expression ''((quote (quote quote)) (quote quote)))
;; These do not satisfy the quote test.
;; (pp-eval-expression ''quote)
;; (pp-eval-expression ''(quote))
;; (pp-eval-expression ''(quote . quote))
;; (pp-eval-expression ''(quote a b))
;; (pp-eval-expression ''(quotefoo))
;; (pp-eval-expression ''(a b))

(provide 'pp)				; so (require 'pp) works

;;; pp.el ends here
