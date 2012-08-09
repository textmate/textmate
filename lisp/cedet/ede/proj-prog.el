;;; ede-proj-prog.el --- EDE Generic Project program support

;; Copyright (C) 1998-2001, 2005, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make

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
;;
;; Handle building programs from object files in and EDE Project file.

(eval-when-compile (require 'cl))
(require 'ede/pmake)
(require 'ede/proj-obj)

(declare-function ede-shell-run-something "ede/shell")

;;; Code:
(defclass ede-proj-target-makefile-program
  (ede-proj-target-makefile-objectcode)
  ((ldlibs-local :initarg :ldlibs-local
		 :initform nil
		 :type list
		 :custom (repeat (string :tag "Local Library"))
		 :documentation
	   "Libraries that are part of this project.
The full path to these libraries should be specified, such as:
../lib/libMylib.la  or ../ar/myArchive.a

Note: Currently only used for Automake projects."
	   )
   (ldflags :initarg :ldflags
	    :initform nil
	    :type list
	    :custom (repeat (string :tag "Link Flag"))
	    :documentation
	    "Additional flags to add when linking this target.
Use this to specify specific options to the linker.
A Common use may be to add -L to specify in-project locations of libraries
specified with ldlibs.")
   (ldlibs :initarg :ldlibs
	   :initform nil
	   :type list
	   :custom (repeat (string :tag "Library"))
	   :documentation
	   "Libraries, such as \"m\" or \"Xt\" which this program depends on.
The linker flag \"-l\" is automatically prepended.  Do not include a \"lib\"
prefix, or a \".so\" suffix.
Use the 'ldflags' slot to specify where in-project libraries might be.

Note: Currently only used for Automake projects."
	   )
   )
   "This target is an executable program.")

(defmethod ede-proj-makefile-insert-automake-pre-variables
  ((this ede-proj-target-makefile-program))
  "Insert bin_PROGRAMS variables needed by target THIS."
  (ede-pmake-insert-variable-shared "bin_PROGRAMS"
    (insert (ede-name this)))
  (call-next-method))

(defmethod ede-proj-makefile-insert-automake-post-variables
  ((this ede-proj-target-makefile-program))
  "Insert bin_PROGRAMS variables needed by target THIS."
  (ede-pmake-insert-variable-shared
      (concat (ede-name this) "_LDADD")
    (mapc (lambda (l) (insert " " l)) (oref this ldlibs-local))
    (mapc (lambda (c) (insert " " c)) (oref this ldflags))
    (when (oref this ldlibs)
      (mapc (lambda (d) (insert " -l" d)) (oref this ldlibs)))
    )
  (call-next-method))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-makefile-program))
  "Insert variables needed by the compiler THIS."
  (call-next-method)
  (let ((lf (mapconcat 'identity (oref this ldflags) " ")))
    (with-slots (ldlibs) this
      (if ldlibs
	  (setq lf
		(concat lf " -l" (mapconcat 'identity ldlibs " -l")))))
    ;; LDFLAGS as needed.
    (when (and lf (not (string= "" lf)))
      (ede-pmake-insert-variable-once "LDDEPS" (insert lf)))))

(defmethod project-debug-target ((obj ede-proj-target-makefile-program))
  "Debug a program target OBJ."
  (let ((tb (get-buffer-create " *padt*"))
	(dd (if (not (string= (oref obj path) ""))
		(oref obj path)
	      default-directory))
	(cmd nil))
    (unwind-protect
	(progn
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (read-from-minibuffer
		     "Run (like this): "
		     (concat (symbol-name ede-debug-program-function)
			     " " (ede-target-name obj))))
	  (funcall ede-debug-program-function cmd))
      (kill-buffer tb))))

(defmethod project-run-target ((obj ede-proj-target-makefile-program) &optional command)
  "Run a program target OBJ.
Optional COMMAND is the command to run in place of asking the user."
  (require 'ede/shell)
  (let ((tb (get-buffer-create " *padt*"))
	(dd (if (not (string= (oref obj path) ""))
		(oref obj path)
	      default-directory))
	(cmd nil))
    (unwind-protect
	(progn
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (or command
			(read-from-minibuffer
			 "Run (like this): "
			 (concat "./" (ede-target-name obj)))))
	  (ede-shell-run-something obj cmd)
	  )
      (kill-buffer tb))))

(provide 'ede/proj-prog)

;;; ede/proj-prog.el ends here
