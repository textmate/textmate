;;; ede-proj-misc.el --- EDE Generic Project Emacs Lisp support

;; Copyright (C) 1998-2001, 2008-2012  Free Software Foundation, Inc.

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
;; Handle miscellaneous compilable projects in and EDE Project file.
;; This misc target lets the user link in custom makefiles to an EDE
;; project.

(eval-when-compile (require 'cl))
(require 'ede/pmake)
(require 'ede/proj-comp)

;;; Code:

;; FIXME this isn't how you spell "miscellaneous". :(
(defclass ede-proj-target-makefile-miscelaneous (ede-proj-target-makefile)
  ((sourcetype :initform '(ede-misc-source))
   (availablecompilers :initform '(ede-misc-compile))
   (submakefile :initarg :submakefile
		:initform ""
		:type string
		:custom string
		:documentation
		"Miscellaneous sources which have a specialized makefile.
The sub-makefile is used to build this target.")
   )
   "Miscellaneous target type.
A user-written makefile is used to build this target.
All listed sources are included in the distribution.")

(defvar ede-misc-source
  (ede-sourcecode "ede-misc-source"
		  :name "Miscellaneous"
		  :sourcepattern ".*")
  "Miscellaneous field definition.")

(defvar ede-misc-compile
  (ede-compiler "ede-misc-compile"
		:name "Sub Makefile"
		:commands
		'(
		  )
		:autoconf nil
		:sourcetype '(ede-misc-source)
		)
  "Compile code via a sub-makefile.")

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-miscelaneous))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_MISC"))

(defmethod ede-proj-makefile-dependency-files
  ((this ede-proj-target-makefile-miscelaneous))
  "Return a list of files which THIS target depends on."
  (with-slots (submakefile) this
    (cond ((string= submakefile "")
	   nil)
	  ((not submakefile)
	   nil)
	  (t (list submakefile)))))

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-makefile-miscelaneous))
  "Create the make rule needed to create an archive for THIS."
  ;; DO NOT call the next method.  We will never have any compilers,
  ;; or any dependencies, or stuff like this.  This rule will let us
  ;; deal with it in a nice way.
  (insert (ede-name this) ": ")
  (with-slots (submakefile) this
    (if (string= submakefile "")
	(insert "\n\t@\n\n")
      (insert submakefile "\n" "\t$(MAKE) -f " submakefile "\n\n"))))

(provide 'ede/proj-misc)

;;; ede/proj-misc.el ends here
