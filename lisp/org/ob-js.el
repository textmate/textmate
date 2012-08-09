;;; ob-js.el --- org-babel functions for Javascript

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, js
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

;; - a non-browser javascript engine such as node.js http://nodejs.org/
;;   or mozrepl http://wiki.github.com/bard/mozrepl/
;;
;; - for session based evaluation mozrepl and moz.el are required see
;;   http://wiki.github.com/bard/mozrepl/emacs-integration for
;;   configuration instructions

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function run-mozilla "ext:moz" (arg))

(defvar org-babel-default-header-args:js '()
  "Default header arguments for js code blocks.")

(defvar org-babel-js-eoe "org-babel-js-eoe"
  "String to indicate that evaluation has completed.")

(defcustom org-babel-js-cmd "node"
  "Name of command used to evaluate js blocks."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defvar org-babel-js-function-wrapper
  "require('sys').print(require('sys').inspect(function(){%s}()));"
  "Javascript code to print value of body.")

(defun org-babel-execute:js (body params)
  "Execute a block of Javascript code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((org-babel-js-cmd (or (cdr (assoc :cmd params)) org-babel-js-cmd))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:js params))))
    (org-babel-js-read
     (if (not (string= (cdr (assoc :session params)) "none"))
	 ;; session evaluation
         (let ((session (org-babel-prep-session:js
			 (cdr (assoc :session params)) params)))
	   (nth 1
		(org-babel-comint-with-output
		    (session (format "%S" org-babel-js-eoe) t body)
		  (mapc
		   (lambda (line)
		     (insert (org-babel-chomp line)) (comint-send-input nil t))
		   (list body (format "%S" org-babel-js-eoe))))))
       ;; external evaluation
       (let ((script-file (org-babel-temp-file "js-script-")))
         (with-temp-file script-file
           (insert
            ;; return the value or the output
            (if (string= result-type "value")
                (format org-babel-js-function-wrapper full-body)
              full-body)))
         (org-babel-eval
	  (format "%s %s" org-babel-js-cmd
		  (org-babel-process-file-name script-file)) ""))))))

(defun org-babel-js-read (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
        (concat "'"
                (replace-regexp-in-string
                 "\\[" "(" (replace-regexp-in-string
                            "\\]" ")" (replace-regexp-in-string
                                       ", " " " (replace-regexp-in-string
						 "'" "\"" results))))))
     results)))

(defun org-babel-js-var-to-js (var)
  "Convert VAR into a js variable.
Convert an elisp value into a string of js source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-js-var-to-js var ", ") "]")
    (format "%S" var)))

(defun org-babel-prep-session:js (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-js-initiate-session session))
	 (var-lines (org-babel-variable-assignments:js params)))
    (when session
      (org-babel-comint-in-buffer session
	(sit-for .5) (goto-char (point-max))
	(mapc (lambda (var)
		(insert var) (comint-send-input nil t)
		(org-babel-comint-wait-for-output session)
		(sit-for .1) (goto-char (point-max))) var-lines)))
    session))

(defun org-babel-variable-assignments:js (params)
  "Return list of Javascript statements assigning the block's variables"
  (mapcar
   (lambda (pair) (format "var %s=%s;"
			  (car pair) (org-babel-js-var-to-js (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-js-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (cond
     ((string= "mozrepl" org-babel-js-cmd)
      (require 'moz)
      (let ((session-buffer (save-window-excursion
			      (run-mozilla nil)
			      (rename-buffer session)
			      (current-buffer))))
	(if (org-babel-comint-buffer-livep session-buffer)
	    (progn (sit-for .25) session-buffer)
	  (sit-for .5)
	  (org-babel-js-initiate-session session))))
     ((string= "node" org-babel-js-cmd )
      (error "session evaluation with node.js is not supported"))
     (t
      (error "sessions are only supported with mozrepl add \":cmd mozrepl\"")))))

(provide 'ob-js)



;;; ob-js.el ends here
