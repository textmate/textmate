;;; ob-org.el --- org-babel functions for org code block evaluation

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

;; This is the simplest of code blocks, where upon evaluation the
;; contents of the code block are returned in a raw result.

;;; Code:
(require 'ob)

(declare-function org-export-string "org-exp" (string fmt &optional dir))

(defvar org-babel-default-header-args:org
  '((:results . "raw silent") (:exports . "results"))
  "Default arguments for evaluating a org source block.")

(defvar org-babel-org-default-header
  "#+TITLE: default empty header\n"
  "Default header inserted during export of org blocks.")

(defun org-babel-expand-body:org (body params)
  (dolist (var (mapcar #'cdr (org-babel-get-header params :var)))
    (setq body (replace-regexp-in-string
		(regexp-quote (format "$%s" (car var)))  (cdr var) body
		nil 'literal)))
  body)

(defun org-babel-execute:org (body params)
  "Execute a block of Org code with.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
	(body (org-babel-expand-body:org
	       (replace-regexp-in-string "^," "" body) params)))
    (cond
     ((member "latex" result-params) (org-export-string
				      (concat "#+Title: \n" body) "latex"))
     ((member "html" result-params)  (org-export-string body "html"))
     ((member "ascii" result-params) (org-export-string body "ascii"))
     (t body))))

(defun org-babel-prep-session:org (session params)
  "Return an error because org does not support sessions."
  (error "Org does not support sessions"))

(provide 'ob-org)



;;; ob-org.el ends here
