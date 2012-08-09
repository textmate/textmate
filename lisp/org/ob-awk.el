;;; ob-awk.el --- org-babel functions for awk evaluation

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

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

;; Babel's awk can use special header argument:
;;
;; - :in-file takes a path to a file of data to be processed by awk
;;
;; - :stdin takes an Org-mode data or code block reference, the value
;;          of which will be passed to the awk process through STDIN

;;; Code:
(require 'ob)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function org-babel-ref-resolve "ob-ref" (ref))
(declare-function orgtbl-to-generic "org-table" (table params))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("awk" . "awk"))

(defvar org-babel-awk-command "awk"
  "Name of the awk executable command.")

(defun org-babel-expand-body:awk (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (dolist (pair (mapcar #'cdr (org-babel-get-header params :var)))
    (setf body (replace-regexp-in-string
                (regexp-quote (format "$%s" (car pair))) (cdr pair) body)))
  body)

(defun org-babel-execute:awk (body params)
  "Execute a block of Awk code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Awk source code block")
  (let* ((result-params (cdr (assoc :result-params params)))
         (cmd-line (cdr (assoc :cmd-line params)))
         (in-file (cdr (assoc :in-file params)))
	 (full-body (org-babel-expand-body:awk body params))
	 (code-file ((lambda (file) (with-temp-file file (insert full-body)) file)
                     (org-babel-temp-file "awk-")))
	 (stdin ((lambda (stdin)
		   (when stdin
		     (let ((tmp (org-babel-temp-file "awk-stdin-"))
			   (res (org-babel-ref-resolve stdin)))
		       (with-temp-file tmp
			 (insert (org-babel-awk-var-to-awk res)))
		       tmp)))
		 (cdr (assoc :stdin params))))
         (cmd (mapconcat #'identity (remove nil (list org-babel-awk-command
						      "-f" code-file
						      cmd-line
						      in-file))
			 " ")))
    (org-babel-reassemble-table
     ((lambda (results)
	(when results
	  (if (or (member "scalar" result-params)
		  (member "verbatim" result-params)
		  (member "output" result-params))
	      results
	    (let ((tmp (org-babel-temp-file "awk-results-")))
	      (with-temp-file tmp (insert results))
	      (org-babel-import-elisp-from-file tmp)))))
      (cond
       (stdin (with-temp-buffer
		(call-process-shell-command cmd stdin (current-buffer))
		(buffer-string)))
       (t (org-babel-eval cmd ""))))
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun org-babel-awk-var-to-awk (var &optional sep)
  "Return a printed value of VAR suitable for parsing with awk."
  (flet ((echo-var (v) (if (stringp v) v (format "%S" v))))
    (cond
     ((and (listp var) (listp (car var)))
      (orgtbl-to-generic var  (list :sep (or sep "\t") :fmt #'echo-var)))
     ((listp var)
      (mapconcat #'echo-var var "\n"))
     (t (echo-var var)))))

(defun org-babel-awk-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape results))

(provide 'ob-awk)



;;; ob-awk.el ends here
