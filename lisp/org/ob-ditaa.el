;;; ob-ditaa.el --- org-babel functions for ditaa evaluation

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

;; Org-Babel support for evaluating ditaa source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in ditaa
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:ditaa
  '((:results . "file") (:exports . "results") (:java . "-Dfile.encoding=UTF-8"))
  "Default arguments for evaluating a ditaa source block.")

(defvar org-ditaa-jar-path)
(defun org-babel-execute:ditaa (body params)
  "Execute a block of Ditaa code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (out-file ((lambda (el)
		      (or el
			  (error
			   "ditaa code block requires :file header argument")))
		    (cdr (assoc :file params))))
	 (cmdline (cdr (assoc :cmdline params)))
	 (java (cdr (assoc :java params)))
	 (in-file (org-babel-temp-file "ditaa-"))
	 (cmd (concat "java " java " -jar "
		      (shell-quote-argument
		       (expand-file-name org-ditaa-jar-path))
		      " " cmdline
		      " " (org-babel-process-file-name in-file)
		      " " (org-babel-process-file-name out-file))))
    (unless (file-exists-p org-ditaa-jar-path)
      (error "Could not find ditaa.jar at %s" org-ditaa-jar-path))
    (with-temp-file in-file (insert body))
    (message cmd) (shell-command cmd)
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:ditaa (session params)
  "Return an error because ditaa does not support sessions."
  (error "Ditaa does not support sessions"))

(provide 'ob-ditaa)



;;; ob-ditaa.el ends here
