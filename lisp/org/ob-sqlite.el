;;; ob-sqlite.el --- org-babel functions for sqlite database interaction

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Eric Schulte
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

;; Org-Babel support for evaluating sqlite source code.

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)

(declare-function org-fill-template "org" (template alist))
(declare-function org-table-convert-region "org-table"
		  (beg0 end0 &optional separator))
(declare-function orgtbl-to-csv "org-table" (TABLE PARAMS))

(defvar org-babel-default-header-args:sqlite '())

(defvar org-babel-header-arg-names:sqlite
  '(db header echo bail csv column html line list separator nullvalue)
  "Sqlite specific header args.")

(defun org-babel-expand-body:sqlite (body params)
  "Expand BODY according to the values of PARAMS."
  (org-babel-sqlite-expand-vars
   body (mapcar #'cdr (org-babel-get-header params :var))))

(defvar org-babel-sqlite3-command "sqlite3")

(defun org-babel-execute:sqlite (body params)
  "Execute a block of Sqlite code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
	(db (cdr (assoc :db params)))
	(separator (cdr (assoc :separator params)))
	(nullvalue (cdr (assoc :nullvalue params)))
	(headers-p (equal "yes" (cdr (assoc :colnames params))))
	(others (delq nil (mapcar
			   (lambda (arg) (car (assoc arg params)))
			   (list :header :echo :bail :column
				 :csv :html :line :list))))
	exit-code)
    (unless db (error "ob-sqlite: can't evaluate without a database."))
    (with-temp-buffer
      (insert
       (org-babel-eval
	(org-fill-template
	 "%cmd %header %separator %nullvalue %others %csv %db "
	 (list
	  (cons "cmd" org-babel-sqlite3-command)
	  (cons "header" (if headers-p "-header" "-noheader"))
	  (cons "separator"
		(if separator (format "-separator %s" separator) ""))
	  (cons "nullvalue"
		(if nullvalue (format "-nullvalue %s" nullvalue) ""))
	  (cons "others"
		(mapconcat
		 (lambda (arg) (format "-%s" (substring (symbol-name arg) 1)))
		 others " "))
	  ;; for easy table parsing, default header type should be -csv
	  (cons "csv" (if (or (member :csv others) (member :column others)
			      (member :line others) (member :list others)
			      (member :html others) separator)
			  ""
			"-csv"))
	  (cons "db " db)))
	;; body of the code block
	(org-babel-expand-body:sqlite body params)))
      (if (or (member "scalar" result-params)
	      (member "verbatim" result-params)
	      (member "html" result-params)
	      (member "code" result-params)
	      (equal (point-min) (point-max)))
	  (buffer-string)
	(org-table-convert-region (point-min) (point-max)
				  (if (or (member :csv others)
					  (member :column others)
					  (member :line others)
					  (member :list others)
					  (member :html others) separator)
				      nil
				    '(4)))
	(org-babel-sqlite-table-or-scalar
	 (org-babel-sqlite-offset-colnames
	  (org-table-to-lisp) headers-p))))))

(defun org-babel-sqlite-expand-vars (body vars)
  "Expand the variables held in VARS in BODY."
  (mapc
   (lambda (pair)
     (setq body
	   (replace-regexp-in-string
	    (format "\$%s" (car pair))
	    ((lambda (val)
	       (if (listp val)
		   ((lambda (data-file)
		      (with-temp-file data-file
			(insert (orgtbl-to-csv
				 val '(:fmt (lambda (el) (if (stringp el)
							el
						      (format "%S" el)))))))
		      data-file)
		    (org-babel-temp-file "sqlite-data-"))
		 (if (stringp val) val (format "%S" val))))
	     (cdr pair))
	    body)))
   vars)
  body)

(defun org-babel-sqlite-table-or-scalar (result)
  "If RESULT looks like a trivial table, then unwrap it."
  (if (and (equal 1 (length result))
	   (equal 1 (length (car result))))
      (org-babel-read (caar result))
    (mapcar (lambda (row)
	      (if (equal 'hline row)
		  'hline
		(mapcar #'org-babel-read row))) result)))

(defun org-babel-sqlite-offset-colnames (table headers-p)
  "If HEADERS-P is non-nil then offset the first row as column names."
  (if headers-p
      (cons (car table) (cons 'hline (cdr table)))
    table))

(defun org-babel-prep-session:sqlite (session params)
  "Raise an error because support for sqlite sessions isn't implemented.
Prepare SESSION according to the header arguments specified in PARAMS."
  (error "sqlite sessions not yet implemented"))

(provide 'ob-sqlite)



;;; ob-sqlite.el ends here
