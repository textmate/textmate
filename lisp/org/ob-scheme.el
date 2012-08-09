;;; ob-scheme.el --- org-babel functions for Scheme

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, scheme
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

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; - a working scheme implementation
;;   (e.g. guile http://www.gnu.org/software/guile/guile.html)
;;
;; - for session based evaluation cmuscheme.el is required which is
;;   included in Emacs

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function run-scheme "ext:cmuscheme" (cmd))

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")

(defvar org-babel-scheme-eoe "org-babel-scheme-eoe"
  "String to indicate that evaluation has completed.")

(defcustom org-babel-scheme-cmd "guile"
  "Name of command used to evaluate scheme blocks."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-expand-body:scheme (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (if (> (length vars) 0)
        (concat "(let ("
                (mapconcat
                 (lambda (var) (format "%S" (print `(,(car var) ',(cdr var)))))
                 vars "\n      ")
                ")\n" body ")")
      body)))

(defvar scheme-program-name)
(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((result-type (cdr (assoc :result-type params)))
	 (org-babel-scheme-cmd (or (cdr (assoc :scheme params))
				   org-babel-scheme-cmd))
         (full-body (org-babel-expand-body:scheme body params)))
    (read
     (if (not (string= (cdr (assoc :session params)) "none"))
         ;; session evaluation
	 (let ((session (org-babel-prep-session:scheme
			 (cdr (assoc :session params)) params)))
	   (org-babel-comint-with-output
	       (session (format "%S" org-babel-scheme-eoe) t body)
	     (mapc
	      (lambda (line)
		(insert (org-babel-chomp line)) (comint-send-input nil t))
	      (list body (format "%S" org-babel-scheme-eoe)))))
       ;; external evaluation
       (let ((script-file (org-babel-temp-file "scheme-script-")))
         (with-temp-file script-file
           (insert
            ;; return the value or the output
            (if (string= result-type "value")
                (format "(display %s)" full-body)
              full-body)))
         (org-babel-eval
	  (format "%s %s" org-babel-scheme-cmd
		  (org-babel-process-file-name script-file)) ""))))))

(defun org-babel-prep-session:scheme (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-scheme-initiate-session session))
	 (vars (mapcar #'cdr (org-babel-get-header params :var)))
	 (var-lines
	  (mapcar
	   (lambda (var) (format "%S" (print `(define ,(car var) ',(cdr var)))))
	   vars)))
    (when session
      (org-babel-comint-in-buffer session
	(sit-for .5) (goto-char (point-max))
	(mapc (lambda (var)
		(insert var) (comint-send-input nil t)
		(org-babel-comint-wait-for-output session)
		(sit-for .1) (goto-char (point-max))) var-lines)))
    session))

(defun org-babel-scheme-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (require 'cmuscheme)
  (unless (string= session "none")
    (let ((session-buffer (save-window-excursion
			    (run-scheme org-babel-scheme-cmd)
			    (rename-buffer session)
			    (current-buffer))))
      (if (org-babel-comint-buffer-livep session-buffer)
	  (progn (sit-for .25) session-buffer)
        (sit-for .5)
        (org-babel-scheme-initiate-session session)))))

(provide 'ob-scheme)



;;; ob-scheme.el ends here
