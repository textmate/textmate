;;; ob-sql.el --- org-babel functions for sql evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

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

;; Org-Babel support for evaluating sql source code.
;; (see also ob-sqlite.el)
;;
;; SQL is somewhat unique in that there are many different engines for
;; the evaluation of sql (Mysql, PostgreSQL, etc...), so much of this
;; file will have to be implemented engine by engine.
;;
;; Also SQL evaluation generally takes place inside of a database.
;;
;; For now lets just allow a generic ':cmdline' header argument.
;;
;; TODO:
;;
;; - support for sessions
;; - add more useful header arguments (user, passwd, database, etc...)
;; - support for more engines (currently only supports mysql)
;; - what's a reasonable way to drop table data into SQL?
;;

;;; Code:
(require 'ob)
(eval-when-compile (require 'cl))

(declare-function org-table-import "org-table" (file arg))
(declare-function orgtbl-to-csv "org-table" (TABLE PARAMS))

(defvar org-babel-default-header-args:sql '())

(defvar org-babel-header-arg-names:sql
  '(engine out-file))

(defun org-babel-expand-body:sql (body params)
  "Expand BODY according to the values of PARAMS."
  (org-babel-sql-expand-vars
   body (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-execute:sql (body params)
  "Execute a block of Sql code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assoc :result-params params)))
         (cmdline (cdr (assoc :cmdline params)))
         (engine (cdr (assoc :engine params)))
         (in-file (org-babel-temp-file "sql-in-"))
         (out-file (or (cdr (assoc :out-file params))
                       (org-babel-temp-file "sql-out-")))
	 (header-delim "")
         (command (case (intern engine)
                    ('msosql (format "osql %s -s \"\t\" -i %s -o %s"
                                     (or cmdline "")
                                     (org-babel-process-file-name in-file)
                                     (org-babel-process-file-name out-file)))
                    ('mysql (format "mysql %s < %s > %s"
                                    (or cmdline "")
				    (org-babel-process-file-name in-file)
				    (org-babel-process-file-name out-file)))
		    ('postgresql (format
				  "psql -A -P footer=off -F \"\t\"  -f %s -o %s %s"
				    (org-babel-process-file-name in-file)
				    (org-babel-process-file-name out-file)
				    (or cmdline "")))
                    (t (error "no support for the %s sql engine" engine)))))
    (with-temp-file in-file
      (insert (org-babel-expand-body:sql body params)))
    (message command)
    (shell-command command)
    (if (or (member "scalar" result-params)
	    (member "verbatim" result-params)
	    (member "html" result-params)
	    (member "code" result-params)
	    (equal (point-min) (point-max)))
	(with-temp-buffer
	  (progn (insert-file-contents-literally out-file) (buffer-string)))
      (with-temp-buffer
	;; need to figure out what the delimiter is for the header row
	(with-temp-buffer
	  (insert-file-contents out-file)
	  (goto-char (point-min))
	  (when (re-search-forward "^\\(-+\\)[^-]" nil t)
	    (setq header-delim (match-string-no-properties 1)))
	  (goto-char (point-max))
	  (forward-char -1)
	  (while (looking-at "\n")
	    (delete-char 1)
	    (goto-char (point-max))
	    (forward-char -1))
	  (write-file out-file))
	(org-table-import out-file '(16))
	(org-babel-reassemble-table
	 (mapcar (lambda (x)
		   (if (string= (car x) header-delim)
		       'hline
		     x))
		 (org-table-to-lisp))
	 (org-babel-pick-name (cdr (assoc :colname-names params))
			      (cdr (assoc :colnames params)))
	 (org-babel-pick-name (cdr (assoc :rowname-names params))
			      (cdr (assoc :rownames params))))))))

(defun org-babel-sql-expand-vars (body vars)
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
		    (org-babel-temp-file "sql-data-"))
		 (if (stringp val) val (format "%S" val))))
	     (cdr pair))
	    body)))
   vars)
  body)

(defun org-babel-prep-session:sql (session params)
  "Raise an error because Sql sessions aren't implemented."
  (error "sql sessions not yet implemented"))

(provide 'ob-sql)



;;; ob-sql.el ends here
