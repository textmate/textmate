;;; rng-util.el --- utility functions for RELAX NG library

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

(defun rng-make-datatypes-uri (uri)
  (if (string-equal uri "")
      ;; The spec doesn't say to do this, but it's perfectly conformant
      ;; and better than using nil, I think.
      'http://relaxng.org/ns/structure/1.0
    (intern uri)))

(defconst rng-xsd-datatypes-uri
  (rng-make-datatypes-uri "http://www.w3.org/2001/XMLSchema-datatypes"))

(defconst rng-builtin-datatypes-uri (rng-make-datatypes-uri ""))

(defun rng-uniquify-eq (list)
  "Destructively remove `eq' duplicates from LIST."
  (and list
       (let ((head list))
	 (while (cdr head)
	   (if (eq (car head) (cadr head))
	       (setcdr head (cddr head)))
	   (setq head (cdr head)))
	 list)))

(defun rng-uniquify-equal (list)
  "Destructively remove `equal' duplicates from LIST."
  (and list
       (let ((head list))
	 (while (cdr head)
	   (if (equal (car head) (cadr head))
	       (setcdr head (cddr head)))
	   (setq head (cdr head)))
	 list)))

(defun rng-blank-p (str) (string-match "\\`[ \t\n\r]*\\'" str))

(defun rng-substq (new old list)
  "Replace first member of LIST (if any) that is `eq' to OLD by NEW.
LIST is not modified."
  (cond ((null list) nil)
	((eq (car list) old)
	 (cons new (cdr list)))
	(t
	 (let ((tail (cons (car list)
			   nil))
	       (rest (cdr list)))
	   (setq list tail)
	   (while rest
	     (let ((item (car rest)))
	       (setq rest (cdr rest))
	       (cond ((eq item old)
		      (setcdr tail
			      (cons new rest))
		      (setq rest nil))
		     (t
		      (setq tail
			    (setcdr tail
				    (cons item nil))))))))
	 list)))

(defun rng-complete-before-point (start table prompt &optional predicate hist)
  "Complete text between START and point.
Replaces the text between START and point with a string chosen using a
completion table and, when needed, input read from the user with the
minibuffer.
Returns the new string if either a complete and unique completion was
determined automatically or input was read from the user.  Otherwise,
returns nil.
TABLE is an alist, a symbol bound to a function or an obarray as with
the function `completing-read'.
PROMPT is the string to prompt with if user input is needed.
PREDICATE is nil or a function as with `completing-read'.
HIST, if non-nil, specifies a history list as with `completing-read'."
  (let* ((orig (buffer-substring-no-properties start (point)))
	 (completion (try-completion orig table predicate)))
    (cond ((not completion)
	   (if (string= orig "")
	       (message "No completions available")
	     (message "No completion for %s" (rng-quote-string orig)))
	   (ding)
	   nil)
	  ((eq completion t) orig)
	  ((not (string= completion orig))
	   (delete-region start (point))
	   (insert completion)
	   (cond ((not (rng-completion-exact-p completion table predicate))
		  (message "Incomplete")
		  nil)
		 ((eq (try-completion completion table predicate) t)
		  completion)
		 (t
		  (message "Complete but not unique")
		  nil)))
	  (t
	   (setq completion
		 (let ((saved-minibuffer-setup-hook
			(default-value 'minibuffer-setup-hook)))
		   (add-hook 'minibuffer-setup-hook
			     'minibuffer-completion-help
			     t)
		   (unwind-protect
		       (completing-read prompt
					table
					predicate
					nil
					orig
					hist)
		     (setq-default minibuffer-setup-hook
				   saved-minibuffer-setup-hook))))
	   (delete-region start (point))
	   (insert completion)
	   completion))))

(defun rng-completion-exact-p (string table predicate)
  (cond ((symbolp table)
	 (funcall table string predicate 'lambda))
	((vectorp table)
	 (intern-soft string table))
	(t (assoc string table))))

(defun rng-quote-string (s)
  (concat "\"" s "\""))

(defun rng-escape-string (s)
  (replace-regexp-in-string "[&\"<>]"
			    (lambda (match)
			      (cdr (assoc match
					  '(("&" . "&amp;")
					    ("\"" . "&quot;")
					    (">" . "&gt;")
					    ("<" . "&lt;")))))
			    s
			    t))

(defun rng-collapse-space (string)
  (setq string
	(replace-regexp-in-string "[ \t\r\n]+" " " string t t))
  (when (string-match "\\` " string)
    (setq string (substring string 1)))
  (when (string-match " \\'" string)
    (setq string (substring string 0 -1)))
  string)

(provide 'rng-util)

;;; rng-util.el ends here
