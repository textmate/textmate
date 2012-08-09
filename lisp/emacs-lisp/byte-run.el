;;; byte-run.el --- byte-compiler support for inlining

;; Copyright (C) 1992, 2001-2012  Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: FSF
;; Keywords: internal
;; Package: emacs

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

;; interface to selectively inlining functions.
;; This only happens when source-code optimization is turned on.

;;; Code:

;; We define macro-declaration-function here because it is needed to
;; handle declarations in macro definitions and this is the first file
;; loaded by loadup.el that uses declarations in macros.

(defun macro-declaration-function (macro decl)
  "Process a declaration found in a macro definition.
This is set as the value of the variable `macro-declaration-function'.
MACRO is the name of the macro being defined.
DECL is a list `(declare ...)' containing the declarations.
The return value of this function is not used."
  ;; We can't use `dolist' or `cadr' yet for bootstrapping reasons.
  (let (d)
    ;; Ignore the first element of `decl' (it's always `declare').
    (while (setq decl (cdr decl))
      (setq d (car decl))
      (if (and (consp d)
	       (listp (cdr d))
	       (null (cdr (cdr d))))
	  (cond ((eq (car d) 'indent)
		 (put macro 'lisp-indent-function (car (cdr d))))
		((eq (car d) 'debug)
		 (put macro 'edebug-form-spec (car (cdr d))))
		((eq (car d) 'doc-string)
		 (put macro 'doc-string-elt (car (cdr d))))
		(t
		 (message "Unknown declaration %s" d)))
	(message "Invalid declaration %s" d)))))


(setq macro-declaration-function 'macro-declaration-function)


;; Redefined in byte-optimize.el.
;; This is not documented--it's not clear that we should promote it.
(fset 'inline 'progn)

;;; Interface to inline functions.

;; (defmacro proclaim-inline (&rest fns)
;;   "Cause the named functions to be open-coded when called from compiled code.
;; They will only be compiled open-coded when byte-compile-optimize is true."
;;   (cons 'eval-and-compile
;; 	(mapcar (lambda (x)
;; 		   (or (memq (get x 'byte-optimizer)
;; 			     '(nil byte-compile-inline-expand))
;; 		       (error
;; 			"%s already has a byte-optimizer, can't make it inline"
;; 			x))
;; 		   (list 'put (list 'quote x)
;; 			 ''byte-optimizer ''byte-compile-inline-expand))
;; 		fns)))

;; (defmacro proclaim-notinline (&rest fns)
;;   "Cause the named functions to no longer be open-coded."
;;   (cons 'eval-and-compile
;; 	(mapcar (lambda (x)
;; 		   (if (eq (get x 'byte-optimizer) 'byte-compile-inline-expand)
;; 		       (put x 'byte-optimizer nil))
;; 		   (list 'if (list 'eq (list 'get (list 'quote x) ''byte-optimizer)
;; 				   ''byte-compile-inline-expand)
;; 			 (list 'put x ''byte-optimizer nil)))
;; 		fns)))

;; This has a special byte-hunk-handler in bytecomp.el.
(defmacro defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'."
  (declare (debug defun))
  (or (memq (get name 'byte-optimizer)
	    '(nil byte-compile-inline-expand))
      (error "`%s' is a primitive" name))
  `(prog1
       (defun ,name ,arglist ,@body)
     (eval-and-compile
       (put ',name 'byte-optimizer 'byte-compile-inline-expand))))

(defvar advertised-signature-table (make-hash-table :test 'eq :weakness 'key))

(defun set-advertised-calling-convention (function signature when)
  "Set the advertised SIGNATURE of FUNCTION.
This will allow the byte-compiler to warn the programmer when she uses
an obsolete calling convention.  WHEN specifies since when the calling
convention was modified."
  (puthash (indirect-function function) signature
           advertised-signature-table))

(defun make-obsolete (obsolete-name current-name &optional when)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)
(set-advertised-calling-convention
 ;; New code should always provide the `when' argument.
 'make-obsolete '(obsolete-name current-name when) "23.1")

(defmacro define-obsolete-function-alias (obsolete-name current-name
						   &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias 'old-fun 'new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias 'old-fun 'new-fun \"old-fun's doc.\")
\(make-obsolete 'old-fun 'new-fun \"22.1\")

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))
(set-advertised-calling-convention
 ;; New code should always provide the `when' argument.
 'define-obsolete-function-alias
 '(obsolete-name current-name when &optional docstring) "23.1")

(defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)
(set-advertised-calling-convention
 ;; New code should always provide the `when' argument.
 'make-obsolete-variable
 '(obsolete-name current-name when &optional access-type) "23.1")

(defmacro define-obsolete-variable-alias (obsolete-name current-name
						 &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
This uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in a .emacs file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

For the benefit of `custom-set-variables', if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
'saved-value, 'saved-variable-comment."
  (declare (doc-string 4))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))
(set-advertised-calling-convention
 ;; New code should always provide the `when' argument.
 'define-obsolete-variable-alias
 '(obsolete-name current-name when &optional docstring) "23.1")

;; FIXME This is only defined in this file because the variable- and
;; function- versions are too.  Unlike those two, this one is not used
;; by the byte-compiler (would be nice if it could warn about obsolete
;; faces, but it doesn't really do anything special with faces).
;; It only really affects M-x describe-face output.
(defmacro define-obsolete-face-alias (obsolete-face current-face when)
  "Make OBSOLETE-FACE a face alias for CURRENT-FACE and mark it obsolete.
The string WHEN gives the Emacs version where OBSOLETE-FACE became
obsolete."
  `(progn
     (put ,obsolete-face 'face-alias ,current-face)
     ;; Used by M-x describe-face.
     (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))

(defmacro dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
  (declare (debug t) (indent 0))
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))


;; interface to evaluating things at compile time and/or load time
;; these macro must come after any uses of them in this file, as their
;; definition in the file overrides the magic definitions on the
;; byte-compile-macro-environment.

(defmacro eval-when-compile (&rest body)
  "Like `progn', but evaluates the body at compile time if you're compiling.
Thus, the result of the body appears to the compiler as a quoted constant.
In interpreted code, this is entirely equivalent to `progn'."
  (declare (debug t) (indent 0))
  ;; Not necessary because we have it in b-c-initial-macro-environment
  ;; (list 'quote (eval (cons 'progn body)))
  (cons 'progn body))

(defmacro eval-and-compile (&rest body)
  "Like `progn', but evaluates the body at compile time and at load time."
  (declare (debug t) (indent 0))
  ;; Remember, it's magic.
  (cons 'progn body))

(put 'with-no-warnings 'lisp-indent-function 0)
(defun with-no-warnings (&rest body)
  "Like `progn', but prevents compiler warnings in the body."
  ;; The implementation for the interpreter is basically trivial.
  (car (last body)))


;; I nuked this because it's not a good idea for users to think of using it.
;; These options are a matter of installation preference, and have nothing to
;; with particular source files; it's a mistake to suggest to users
;; they should associate these with particular source files.
;; There is hardly any reason to change these parameters, anyway.
;; --rms.

;; (put 'byte-compiler-options 'lisp-indent-function 0)
;; (defmacro byte-compiler-options (&rest args)
;;   "Set some compilation-parameters for this file.  This will affect only the
;; file in which it appears; this does nothing when evaluated, and when loaded
;; from a .el file.
;;
;; Each argument to this macro must be a list of a key and a value.
;;
;;   Keys:		  Values:		Corresponding variable:
;;
;;   verbose	  t, nil		byte-compile-verbose
;;   optimize	  t, nil, source, byte	byte-compile-optimize
;;   warnings	  list of warnings	byte-compile-warnings
;; 		      Valid elements: (callargs redefine free-vars unresolved)
;;   file-format	  emacs18, emacs19	byte-compile-compatibility
;;
;; For example, this might appear at the top of a source file:
;;
;;     (byte-compiler-options
;;       (optimize t)
;;       (warnings (- free-vars))		; Don't warn about free variables
;;       (file-format emacs19))"
;;   nil)

;;; byte-run.el ends here
