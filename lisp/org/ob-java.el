;;; ob-java.el --- org-babel functions for java evaluation

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

;; Currently this only supports the external compilation and execution
;; of java code blocks (i.e., no session support).

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("java" . "java"))

(defvar org-babel-java-command "java"
  "Name of the java command.")

(defvar org-babel-java-compiler "javac"
  "Name of the java compiler.")

(defun org-babel-execute:java (body params)
  (let* ((classname (or (cdr (assoc :classname params))
			(error
			 "Can't compile a java block without a classname")))
	 (packagename (file-name-directory classname))
	 (src-file (concat classname ".java"))
	 (cmpflag (or (cdr (assoc :cmpflag params)) ""))
	 (cmdline (or (cdr (assoc :cmdline params)) ""))
	 (full-body (org-babel-expand-body:generic body params))
	 (compile
	  (progn (with-temp-file src-file (insert full-body))
		 (org-babel-eval
		  (concat org-babel-java-compiler
			  " " cmpflag " " src-file) ""))))
    ;; created package-name directories if missing
    (unless (or (not packagename) (file-exists-p packagename))
      (make-directory packagename 'parents))
    ((lambda (results)
       (org-babel-reassemble-table
	(if (member "vector" (cdr (assoc :result-params params)))
	    (let ((tmp-file (org-babel-temp-file "c-")))
	      (with-temp-file tmp-file (insert results))
	      (org-babel-import-elisp-from-file tmp-file))
	  (org-babel-read results))
	(org-babel-pick-name
	 (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
	(org-babel-pick-name
	 (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))
     (org-babel-eval (concat org-babel-java-command
			     " " cmdline " " classname) ""))))

(provide 'ob-java)



;;; ob-java.el ends here
