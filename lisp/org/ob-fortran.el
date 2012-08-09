;;; ob-fortran.el --- org-babel functions for fortran

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Authors: Sergey Litvinov
;;       Eric Schulte
;; Keywords: literate programming, reproducible research, fortran
;; Homepage: http://orgmode.org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating fortran code.

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'cc-mode)

(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("fortran" . "F90"))

(defvar org-babel-default-header-args:fortran '())

(defvar org-babel-fortran-compiler "gfortran"
  "fortran command used to compile a fortran source code file into an
  executable.")

(defun org-babel-execute:fortran (body params)
  "This function should only be called by `org-babel-execute:fortran'"
  (let* ((tmp-src-file (org-babel-temp-file "fortran-src-" ".F90"))
         (tmp-bin-file (org-babel-temp-file "fortran-bin-"))
         (cmdline (cdr (assoc :cmdline params)))
         (flags (cdr (assoc :flags params)))
         (full-body (org-babel-expand-body:fortran body params))
         (compile
	  (progn
	    (with-temp-file tmp-src-file (insert full-body))
	    (org-babel-eval
	     (format "%s -o %s %s %s"
		     org-babel-fortran-compiler
		     (org-babel-process-file-name tmp-bin-file)
		     (mapconcat 'identity
				(if (listp flags) flags (list flags)) " ")
		     (org-babel-process-file-name tmp-src-file)) ""))))
    ((lambda (results)
       (org-babel-reassemble-table
	(if (member "vector" (cdr (assoc :result-params params)))
	    (let ((tmp-file (org-babel-temp-file "f-")))
	      (with-temp-file tmp-file (insert results))
	      (org-babel-import-elisp-from-file tmp-file))
	  (org-babel-read results))
	(org-babel-pick-name
	 (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
	(org-babel-pick-name
	 (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))
     (org-babel-trim
       (org-babel-eval
	(concat tmp-bin-file (if cmdline (concat " " cmdline) "")) "")))))

(defun org-babel-expand-body:fortran (body params)
  "Expand a block of fortran or fortran code with org-babel according to
it's header arguments."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
        (main-p (not (string= (cdr (assoc :main params)) "no")))
        (includes (or (cdr (assoc :includes params))
                      (org-babel-read (org-entry-get nil "includes" t))))
        (defines (org-babel-read
                  (or (cdr (assoc :defines params))
                      (org-babel-read (org-entry-get nil "defines" t))))))
     (mapconcat 'identity
		(list
		 ;; includes
		 (mapconcat
		  (lambda (inc) (format "#include %s" inc))
		  (if (listp includes) includes (list includes)) "\n")
		 ;; defines
		 (mapconcat
		  (lambda (inc) (format "#define %s" inc))
		  (if (listp defines) defines (list defines)) "\n")
		 ;; body
		 (if main-p
		     (org-babel-fortran-ensure-main-wrap
		      (concat
		       ;; variables
		       (mapconcat 'org-babel-fortran-var-to-fortran vars "\n")
		       body) params)
		   body) "\n") "\n")))

(defun org-babel-fortran-ensure-main-wrap (body params)
  "Wrap body in a \"program ... end program\" block if none exists."
  (if (string-match "^[ \t]*program[ \t]*.*" (capitalize body))
       (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
	 (if vars (error "cannot use :vars if 'program' statement is present"))
	 body)
    (format "program main\n%s\nend program main\n" body)))

(defun org-babel-prep-session:fortran (session params)
  "This function does nothing as fortran is a compiled language with no
support for sessions"
  (error "fortran is a compiled languages -- no support for sessions"))

(defun org-babel-load-session:fortran (session body params)
  "This function does nothing as fortran is a compiled language with no
support for sessions"
  (error "fortran is a compiled languages -- no support for sessions"))

;; helper functions

(defun org-babel-fortran-var-to-fortran (pair)
  "Convert an elisp val into a string of fortran code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (cond
     ((integerp val)
      (format "integer, parameter  ::  %S = %S\n" var val))
     ((floatp val)
      (format "real, parameter ::  %S = %S\n" var val))
     ((or (integerp val))
      (format "character, parameter :: %S = '%S'\n" var val))
     ((stringp val)
      (format "character(len=%d), parameter ::  %S = '%s'\n"
              (length val) var val))
     ((listp val)
      (format "real, parameter :: %S(%d) = %s\n"
	      var (length val) (ob-fortran-transform-list val)))
     (t
      (error (format "the type of parameter %s is not supported by ob-fortran"
		     var))))))

(defun ob-fortran-transform-list (val)
  "Return a fortran representation of enclose syntactic lists."
  (if (listp val)
      (concat "(/" (mapconcat #'ob-fortran-transform-list val ", ") "/)")
    (format "%S" val)))

(provide 'ob-fortran)

;;; ob-fortran.el ends here
