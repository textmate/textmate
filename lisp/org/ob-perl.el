;;; ob-perl.el --- org-babel functions for perl evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Authors: Dan Davison
;;	 Eric Schulte
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

;; Org-Babel support for evaluating perl source code.

;;; Code:
(require 'ob)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("perl" . "pl"))

(defvar org-babel-default-header-args:perl '())

(defvar org-babel-perl-command "perl"
  "Name of command to use for executing perl code.")

(defun org-babel-execute:perl (body params)
  "Execute a block of Perl code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (cdr (assoc :session params)))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:perl params)))
	(session (org-babel-perl-initiate-session session)))
    (org-babel-reassemble-table
     (org-babel-perl-evaluate session full-body result-type)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun org-babel-prep-session:perl (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for Perl."))

(defun org-babel-variable-assignments:perl (params)
  "Return list of perl statements assigning the block's variables"
  (mapcar
   (lambda (pair)
     (format "$%s=%s;"
	     (car pair)
	     (org-babel-perl-var-to-perl (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

;; helper functions

(defun org-babel-perl-var-to-perl (var)
  "Convert an elisp value to a perl variable.
The elisp value, VAR, is converted to a string of perl source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-perl-var-to-perl var ", ") "]")
    (format "%S" var)))

(defvar org-babel-perl-buffers '(:default . nil))

(defun org-babel-perl-initiate-session (&optional session params)
  "Return nil because sessions are not supported by perl"
nil)

(defvar org-babel-perl-wrapper-method
  "
sub main {
%s
}
@r = main;
open(o, \">%s\");
print o join(\"\\n\", @r), \"\\n\"")

(defvar org-babel-perl-pp-wrapper-method
  nil)

(defun org-babel-perl-evaluate (session body &optional result-type)
  "Pass BODY to the Perl process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Perl."))
  (case result-type
    (output (org-babel-eval org-babel-perl-command body))
    (value (let ((tmp-file (org-babel-temp-file "perl-")))
	     (org-babel-eval
	      org-babel-perl-command
	      (format org-babel-perl-wrapper-method body
		      (org-babel-process-file-name tmp-file 'noquote)))
	     (org-babel-eval-read-file tmp-file)))))

(provide 'ob-perl)



;;; ob-perl.el ends here
