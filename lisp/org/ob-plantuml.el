;;; ob-plantuml.el --- org-babel functions for plantuml evaluation

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Zhang Weize
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

;; Org-Babel support for evaluating plantuml script.
;;
;; Inspired by Ian Yang's org-export-blocks-format-plantuml
;; http://www.emacswiki.org/emacs/org-export-blocks-format-plantuml.el

;;; Requirements:

;; plantuml     | http://plantuml.sourceforge.net/
;; plantuml.jar | `org-plantuml-jar-path' should point to the jar file

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:plantuml
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a plantuml source block.")

(defcustom org-plantuml-jar-path nil
  "Path to the plantuml.jar file."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-execute:plantuml (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (out-file (or (cdr (assoc :file params))
		       (error "plantuml requires a \":file\" header argument")))
	 (cmdline (cdr (assoc :cmdline params)))
	 (in-file (org-babel-temp-file "plantuml-"))
	 (java (or (cdr (assoc :java params)) ""))
	 (cmd (if (not org-plantuml-jar-path)
		  (error "`org-plantuml-jar-path' is not set")
		(concat "java " java " -jar "
			(shell-quote-argument
			 (expand-file-name org-plantuml-jar-path))
			(if (string= (file-name-extension out-file) "svg")
			    " -tsvg" "")
			(if (string= (file-name-extension out-file) "eps")
			    " -teps" "")
			" -p " cmdline " < "
			(org-babel-process-file-name in-file)
			" > "
			(org-babel-process-file-name out-file)))))
    (unless (file-exists-p org-plantuml-jar-path)
      (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
    (with-temp-file in-file (insert (concat "@startuml\n" body "\n@enduml")))
    (message "%s" cmd) (org-babel-eval cmd "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:plantuml (session params)
  "Return an error because plantuml does not support sessions."
  (error "Plantuml does not support sessions"))

(provide 'ob-plantuml)



;;; ob-plantuml.el ends here
