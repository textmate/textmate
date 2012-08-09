;;; ede/proj-comp.el --- EDE Generic Project compiler/rule driver

;; Copyright (C) 1999-2001, 2004-2005, 2007, 2009-2012
;;   Free Software Foundation, Inc.

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
;; This software handles the maintenance of compiler and rule definitions
;; for different object types.
;;
;; The `ede-compiler' class lets different types of project objects create
;; definitions of compilers that can be swapped in and out for compiling
;; source code.  Users can also define new compiler types whenever they
;; some customized behavior.
;;
;; The `ede-makefile-rule' class lets users add customized rules into their
;; objects, and also lets different compilers add chaining rules to their
;; behaviors.
;;
;; It is important that all new compiler types be registered once.  That
;; way the chaining rules and variables are inserted into any given Makefile
;; only once.
;;
;; To insert many compiler elements, wrap them in `ede-compiler-begin-unique'
;; before calling their insert methods.
;; To write a method that inserts a variable or rule for a compiler
;; based object, wrap the body of your call in `ede-compiler-only-once'

(eval-when-compile (require 'cl))
(require 'ede)				;source object
(require 'ede/autoconf-edit)

;;; Types:
(defclass ede-compilation-program (eieio-instance-inheritor)
  ((name :initarg :name
	 :type string
	 :custom string
	 :documentation "Name of this type of compiler.")
   (variables :initarg :variables
	      :type list
	      :custom (repeat (cons (string :tag "Variable")
				    (string :tag "Value")))
	      :documentation
	      "Variables needed in the Makefile for this compiler.
An assoc list where each element is (VARNAME . VALUE) where VARNAME
is a string, and VALUE is either a string, or a list of strings.
For example, GCC would define CC=gcc, and emacs would define EMACS=emacs.")
   (sourcetype :initarg :sourcetype
	       :type list ;; of symbols
	       :documentation
	       "A list of `ede-sourcecode' objects this class will handle.
This is used to match target objects with the compilers and linkers
they can use, and which files this object is interested in."
	       :accessor ede-object-sourcecode)
   (rules :initarg :rules
	  :initform nil
	  :type list
	  :custom (repeat (object :objecttype ede-makefile-rule))
	  :documentation
	  "Auxiliary rules needed for this compiler to run.
For example, yacc/lex files need additional chain rules, or inferences.")
   (commands :initarg :commands
	    :type list
	    :custom (repeat string)
	    :documentation
	    "The commands used to execute this compiler.
The object which uses this compiler will place these commands after
its rule definition.")
   (autoconf :initarg :autoconf
	     :initform nil
	     :type list
	     :custom (repeat string)
	     :documentation
	     "Autoconf function to call if this type of compiler is used.
When a project is in Automake mode, this defines the autoconf function to
call to initialize automake to use this compiler.
For example, there may be multiple C compilers, but they all probably
use the same autoconf form.")
   (objectextention :initarg :objectextention
		    :type string
		    :documentation
		    "A string which is the extension used for object files.
For example, C code uses .o on Unix, and Emacs Lisp uses .elc.")
   )
  "A program used to compile or link a program via a Makefile.
Contains everything needed to output code into a Makefile, or autoconf
file.")

(defclass ede-compiler (ede-compilation-program)
  ((makedepends :initarg :makedepends
		:initform nil
		:type boolean
		:documentation
		"Non-nil if this compiler can make dependencies.")
   (uselinker :initarg :uselinker
	      :initform nil
	      :type boolean
	      :documentation
	      "Non-nil if this compiler creates code that can be linked.
This requires that the containing target also define a list of available
linkers that can be used.")
   )
  "Definition for a compiler.
Different types of objects will provide different compilers for
different situations.")

(defclass ede-linker (ede-compilation-program)
  ()
  "Contains information needed to link many generated object files together.")

(defclass ede-makefile-rule ()
  ((target :initarg :target
	   :initform ""
	   :type string
	   :custom string
	   :documentation "The target pattern.
A pattern of \"%.o\" is used for inference rules, and would match object files.
A target of \"foo.o\" explicitly matches the file foo.o.")
   (dependencies :initarg :dependencies
		 :initform ""
		 :type string
		 :custom string
		 :documentation "Dependencies on this target.
A pattern of \"%.o\" would match a file of the same prefix as the target
if that target is also an inference rule pattern.
A dependency of \"foo.c\" explicitly lists foo.c as a dependency.
A variable such as $(name_SOURCES) will list all the source files
belonging to the target name.")
   (rules :initarg :rules
	  :initform nil
	  :type list
	  :custom (repeat string)
	  :documentation "Scripts to execute.
These scripts will be executed in sh (Unless the SHELL variable is overridden).
Do not prefix with TAB.
Each individual element of this list can be either a string, or
a lambda function.  (The custom element does not yet express that.")
   (phony :initarg :phony
	  :initform nil
	  :type boolean
	  :custom boolean
	  :documentation "Is this a phony rule?
Adds this rule to a .PHONY list."))
  "A single rule for building some target.")

;;; Code:
(defvar ede-compiler-list nil
  "The master list of all EDE compilers.")

(defvar ede-linker-list nil
  "The master list of all EDE compilers.")

(defvar ede-current-build-list nil
  "List of EDE compilers that have already inserted parts of themselves.
This is used when creating a Makefile to prevent duplicate variables and
rules from being created.")

(defmethod initialize-instance :AFTER ((this ede-compiler) &rest fields)
  "Make sure that all ede compiler objects are cached in
`ede-compiler-list'."
  (add-to-list 'ede-compiler-list this))

(defmethod initialize-instance :AFTER ((this ede-linker) &rest fields)
  "Make sure that all ede compiler objects are cached in
`ede-linker-list'."
  (add-to-list 'ede-linker-list this))

(defmacro ede-compiler-begin-unique (&rest body)
  "Execute BODY, making sure that `ede-current-build-list' is maintained.
This will prevent rules from creating duplicate variables or rules."
  `(let ((ede-current-build-list nil))
    ,@body))

(defmacro ede-compiler-only-once (object &rest body)
  "Using OBJECT, execute BODY only once per Makefile generation."
  `(if (not (member ,object ede-current-build-list))
       (progn
	 (add-to-list 'ede-current-build-list ,object)
	 ,@body)))

(defmacro ede-linker-begin-unique (&rest body)
  "Execute BODY, making sure that `ede-current-build-list' is maintained.
This will prevent rules from creating duplicate variables or rules."
  `(let ((ede-current-build-list nil))
    ,@body))

(defmacro ede-linker-only-once (object &rest body)
  "Using OBJECT, execute BODY only once per Makefile generation."
  `(if (not (member ,object ede-current-build-list))
       (progn
	 (add-to-list 'ede-current-build-list ,object)
	 ,@body)))

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec ede-compiler-begin-unique def-body)
	    (def-edebug-spec ede-compiler-only-once (form def-body))
	    (def-edebug-spec ede-linker-begin-unique def-body)
	    (def-edebug-spec ede-linker-only-once (form def-body))
	    (def-edebug-spec ede-pmake-insert-variable-shared (form def-body))
	    ))

;;; Querys
(defun ede-proj-find-compiler (compilers sourcetype)
  "Return a compiler from the list COMPILERS that will compile SOURCETYPE."
  (while (and compilers
	      (not (member sourcetype (oref (car compilers) sourcetype))))
    (setq compilers (cdr compilers)))
  (car-safe compilers))

(defun ede-proj-find-linker (linkers sourcetype)
  "Return a compiler from the list LINKERS to be used with SOURCETYPE."
  (while (and linkers
	      (slot-boundp (car linkers) 'sourcetype)
	      (not (member sourcetype (oref (car linkers) sourcetype))))
    (setq linkers (cdr linkers)))
  (car-safe linkers))

;;; Methods:
(defmethod ede-proj-tweak-autoconf ((this ede-compilation-program))
  "Tweak the configure file (current buffer) to accommodate THIS."
  (mapcar
   (lambda (obj)
     (cond ((stringp obj)
	      (autoconf-insert-new-macro obj))
	     ((consp obj)
	      (autoconf-insert-new-macro (car obj) (cdr obj)))
	     (t (error "Autoconf directives must be a string, or cons cell")))
     )
   (oref this autoconf)))

(defmethod ede-proj-flush-autoconf ((this ede-compilation-program))
  "Flush the configure file (current buffer) to accommodate THIS."
  nil)

(defmacro proj-comp-insert-variable-once (varname &rest body)
  "Add VARNAME into the current Makefile if it doesn't exist.
Execute BODY in a location where a value can be placed."
  `(let ((addcr t) (v ,varname))
     (unless (re-search-backward (concat "^" v "\\s-*=") nil t)
       (insert v "=")
       ,@body
       (if addcr (insert "\n"))
       (goto-char (point-max)))
     ))
(put 'proj-comp-insert-variable-once 'lisp-indent-function 1)

(defmethod ede-proj-makefile-insert-variables ((this ede-compilation-program))
  "Insert variables needed by the compiler THIS."
  (if (eieio-instance-inheritor-slot-boundp this 'variables)
      (with-slots (variables) this
	(mapcar
	 (lambda (var)
	   (proj-comp-insert-variable-once (car var)
	     (let ((cd (cdr var)))
	       (if (listp cd)
		   (mapc (lambda (c) (insert " " c)) cd)
		 (insert cd)))))
	 variables))))

(defmethod ede-compiler-intermediate-objects-p ((this ede-compiler))
  "Return non-nil if THIS has intermediate object files.
If this compiler creates code that can be linked together,
then the object files created by the compiler are considered intermediate."
  (oref this uselinker))

(defmethod ede-compiler-intermediate-object-variable ((this ede-compiler)
						      targetname)
  "Return a string based on THIS representing a make object variable.
TARGETNAME is the name of the target that these objects belong to."
  (concat targetname "_OBJ"))

(defmethod ede-proj-makefile-insert-object-variables ((this ede-compiler)
						      targetname sourcefiles)
  "Insert an OBJ variable to specify object code to be generated for THIS.
The name of the target is TARGETNAME as a string.  SOURCEFILES is the list of
files to be objectified.
Not all compilers do this."
  (if (ede-compiler-intermediate-objects-p this)
      (progn
	(insert (ede-compiler-intermediate-object-variable this targetname)
		"=")
	(let ((src (oref this sourcetype)))
	  (mapc (lambda (s)
		  (let ((ts src))
		    (while (and ts (not (ede-want-file-source-p
					 (symbol-value (car ts)) s)))
		      (setq ts (cdr ts)))
		    ;; Only insert the object if the given file is a major
		    ;; source-code type.
		    (if ts;; a match as a source file.
			(insert " " (file-name-sans-extension s)
				(oref this objectextention)))))
		sourcefiles)
	  (insert "\n")))))

(defmethod ede-proj-makefile-insert-rules ((this ede-compilation-program))
  "Insert rules needed for THIS compiler object."
  (ede-compiler-only-once this
    (mapc 'ede-proj-makefile-insert-rules (oref this rules))))

(defmethod ede-proj-makefile-insert-rules ((this ede-makefile-rule))
  "Insert rules needed for THIS rule object."
  (if (oref this phony) (insert ".PHONY: (oref this target)\n"))
  (insert (oref this target) ": " (oref this dependencies) "\n\t"
	  (mapconcat (lambda (c) c) (oref this rules) "\n\t")
	  "\n\n"))

(defmethod ede-proj-makefile-insert-commands ((this ede-compilation-program))
  "Insert the commands needed to use compiler THIS.
The object creating makefile rules must call this method for the
compiler it decides to use after inserting in the rule."
  (when (slot-boundp this 'commands)
    (with-slots (commands) this
      (mapc
       (lambda (obj) (insert "\t"
			     (cond ((stringp obj)
				    obj)
				   ((and (listp obj)
					 (eq (car obj) 'lambda))
				    (funcall obj))
				   (t
				    (format "%S" obj)))
			     "\n"))
       commands))
    (insert "\n")))

;;; Some details about our new macro
;;
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec ede-compiler-begin-unique def-body)))
(put 'ede-compiler-begin-unique 'lisp-indent-function 0)
(put 'ede-compiler-only-once 'lisp-indent-function 1)
(put 'ede-linker-begin-unique 'lisp-indent-function 0)
(put 'ede-linker-only-once 'lisp-indent-function 1)

(provide 'ede/proj-comp)

;;; ede/proj-comp.el ends here
