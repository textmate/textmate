;;; ob-lisp.el --- org-babel functions for common lisp evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Authors: Joel Boehland
;;	 Eric Schulte
;;	 David T. O'Toole <dto@gnu.org>
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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

;;; support for evaluating common lisp code, relies on slime for all eval

;;; Requirements:

;; Requires SLIME (Superior Lisp Interaction Mode for Emacs.)
;; See http://common-lisp.net/project/slime/

;;; Code:
(require 'ob)

(declare-function slime-eval "ext:slime" (sexp &optional package))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("lisp" . "lisp"))

(defvar org-babel-default-header-args:lisp '())
(defvar org-babel-header-arg-names:lisp '(package))

(defcustom org-babel-lisp-dir-fmt
  "(let ((*default-pathname-defaults* #P%S)) %%s)"
  "Format string used to wrap code bodies to set the current directory.
For example a value of \"(progn ;; %s\\n   %%s)\" would ignore the
current directory string."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-expand-body:lisp (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (mapcar #'cdr (org-babel-get-header params :var)))
	 (result-params (cdr (assoc :result-params params)))
	 (print-level nil) (print-length nil)
	 (body (org-babel-trim
		(if (> (length vars) 0)
		    (concat "(let ("
			    (mapconcat
			     (lambda (var)
			       (format "(%S (quote %S))" (car var) (cdr var)))
			     vars "\n      ")
			    ")\n" body ")")
		  body))))
    (if (or (member "code" result-params)
	    (member "pp" result-params))
	(format "(pprint %s)" body)
      body)))

(defun org-babel-execute:lisp (body params)
  "Execute a block of Common Lisp code with Babel."
  (require 'slime)
  (org-babel-reassemble-table
   ((lambda (result)
      (if (member "output" (cdr (assoc :result-params params)))
	  (car result)
	(condition-case nil
	    (read (org-babel-lisp-vector-to-list (cadr result)))
	  (error (cadr result)))))
    (with-temp-buffer
      (insert (org-babel-expand-body:lisp body params))
      (slime-eval `(swank:eval-and-grab-output
		    ,(let ((dir (if (assoc :dir params)
					    (cdr (assoc :dir params))
					  default-directory)))
		       (format
			(if dir (format org-babel-lisp-dir-fmt dir) "(progn %s)")
			(buffer-substring-no-properties
			 (point-min) (point-max)))))
		  (cdr (assoc :package params)))))
   (org-babel-pick-name (cdr (assoc :colname-names params))
			(cdr (assoc :colnames params)))
   (org-babel-pick-name (cdr (assoc :rowname-names params))
			(cdr (assoc :rownames params)))))

(defun org-babel-lisp-vector-to-list (results)
  ;; TODO: better would be to replace #(...) with [...]
  (replace-regexp-in-string "#(" "(" results))

(provide 'ob-lisp)



;;; ob-lisp.el ends here
