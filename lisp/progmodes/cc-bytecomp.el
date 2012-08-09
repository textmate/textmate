;;; cc-bytecomp.el --- compile time setup for proper compilation

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author:     Martin Stjernholm
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    15-Jul-2000
;; Keywords:   c languages
;; Package:    cc-mode

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

;; This file is used to ensure that the CC Mode files are correctly
;; compiled regardless the environment (e.g. if an older CC Mode with
;; outdated macros are loaded during compilation).  It also provides
;; features to defeat the compiler warnings for selected symbols.
;;
;; There's really nothing CC Mode specific here; this functionality
;; ought to be provided by the byte compilers or some accompanying
;; library.  To use it from some package "foo.el", begin by putting
;; the following blurb at the top of the file:
;;
;;   (eval-when-compile
;;     (let ((load-path
;;            (if (and (boundp 'byte-compile-dest-file)
;;                     (stringp byte-compile-dest-file))
;;                (cons (file-name-directory byte-compile-dest-file) load-path)
;;              load-path)))
;;       (load "cc-bytecomp" nil t))
;;
;; This (unfortunately rather clumsy) form will ensure that the
;; cc-bytecomp.el in the same directory as foo.el is loaded during
;; byte compilation of the latter.
;;
;; At the end of foo.el there should normally be a "(provide 'foo)".
;; Replace it with "(cc-provide 'foo)"; that is necessary to restore
;; the environment after the byte compilation.  If you don't have a
;; `provide' at the end, you have to add the following as the very
;; last form in the file:
;;
;;   (eval-when-compile (cc-bytecomp-restore-environment))
;;
;; Now everything is set to use the various functions and macros in
;; this package.
;;
;; If your package is split into several files, you should use
;; `cc-require', `cc-require-when-compile' or `cc-load' to load them.
;; That ensures that the files in the same directory always are
;; loaded, to avoid mixup with other versions of them that might exist
;; elsewhere in the load path.
;;
;; To suppress byte compiler warnings, use the macros
;; `cc-bytecomp-defun', `cc-bytecomp-defvar',
;; `cc-bytecomp-obsolete-fun', and `cc-bytecomp-obsolete-var'.
;;
;; This file is not used at all after the package has been byte
;; compiled.  It is however necessary when running uncompiled.


;;; Code:

(defvar cc-bytecomp-unbound-variables nil)
(defvar cc-bytecomp-original-functions nil)
(defvar cc-bytecomp-original-properties nil)
(defvar cc-bytecomp-loaded-files nil)
(defvar cc-bytecomp-environment-set nil)

(defmacro cc-bytecomp-debug-msg (&rest args)
  ;;`(message ,@args)
  )

(defun cc-bytecomp-setup-environment ()
  ;; Eval'ed during compilation to setup variables, functions etc
  ;; declared with `cc-bytecomp-defvar' et al.
  (if (not load-in-progress)
      ;; Look at `load-in-progress' to tell whether we're called
      ;; directly in the file being compiled or just from some file
      ;; being loaded during compilation.
      (let (p)
	(if cc-bytecomp-environment-set
	    (error "Byte compilation environment already set - \
perhaps a `cc-bytecomp-restore-environment' is forgotten somewhere"))
	(setq p cc-bytecomp-unbound-variables)
	(while p
	  (if (not (boundp (car p)))
	      (progn
		(eval `(defvar ,(car p)))
		(set (car p) (intern (concat "cc-bytecomp-ignore-var:"
					     (symbol-name (car p)))))
		(cc-bytecomp-debug-msg
		 "cc-bytecomp-setup-environment: Covered variable %s"
		 (car p))))
	  (setq p (cdr p)))
	(setq p cc-bytecomp-original-functions)
	(while p
	  (let ((fun (car (car p)))
		(temp-macro (car (cdr (car p)))))
	    (if (not (fboundp fun))
		(if temp-macro
		    (progn
		      (eval `(defmacro ,fun ,@temp-macro))
		      (cc-bytecomp-debug-msg
		       "cc-bytecomp-setup-environment: Bound macro %s" fun))
		  (fset fun (intern (concat "cc-bytecomp-ignore-fun:"
					    (symbol-name fun))))
		  (cc-bytecomp-debug-msg
		   "cc-bytecomp-setup-environment: Covered function %s" fun))))
	  (setq p (cdr p)))
	(setq p cc-bytecomp-original-properties)
	(while p
	  (let ((sym (car (car (car p))))
		(prop (cdr (car (car p))))
		(tempdef (car (cdr (car p)))))
	    (put sym prop tempdef)
	    (cc-bytecomp-debug-msg
	     "cc-bytecomp-setup-environment: Bound property %s for %s to %s"
	     prop sym tempdef))
	  (setq p (cdr p)))
	(setq cc-bytecomp-environment-set t)
	(cc-bytecomp-debug-msg
	 "cc-bytecomp-setup-environment: Done"))))

(defun cc-bytecomp-restore-environment ()
  ;; Eval'ed during compilation to restore variables, functions etc
  ;; declared with `cc-bytecomp-defvar' et al.
  (if (not load-in-progress)
      (let (p)
	(setq p cc-bytecomp-unbound-variables)
	(while p
	  (let ((var (car p)))
	    (if (boundp var)
		(if (eq (intern (concat "cc-bytecomp-ignore-var:"
					(symbol-name var)))
			(symbol-value var))
		    (progn
		      (makunbound var)
		      (cc-bytecomp-debug-msg
		       "cc-bytecomp-restore-environment: Unbound variable %s"
		       var))
		  (cc-bytecomp-debug-msg
		   "cc-bytecomp-restore-environment: Not restoring variable %s"
		   var))))
	  (setq p (cdr p)))
	(setq p cc-bytecomp-original-functions)
	(while p
	  (let ((fun (car (car p)))
		(temp-macro (car (cdr (car p))))
		(def (car (cdr (cdr (car p))))))
	    (if (fboundp fun)
		(if (eq (or temp-macro
			    (intern (concat "cc-bytecomp-ignore-fun:"
					    (symbol-name fun))))
			   (symbol-function fun))
		    (if (eq def 'unbound)
			(progn
			  (fmakunbound fun)
			  (cc-bytecomp-debug-msg
			   "cc-bytecomp-restore-environment: Unbound function %s"
			   fun))
		      (fset fun def)
		      (cc-bytecomp-debug-msg
		       "cc-bytecomp-restore-environment: Restored function %s"
		       fun))
		  (cc-bytecomp-debug-msg
		   "cc-bytecomp-restore-environment: Not restoring function %s"
		   fun))))
	  (setq p (cdr p)))
	(setq p cc-bytecomp-original-properties)
	(while p
	  (let ((sym (car (car (car p))))
		(prop (cdr (car (car p))))
		(tempdef (car (cdr (car p))))
		(origdef (cdr (cdr (car p)))))
	    (if (eq (get sym prop) tempdef)
		(progn
		  (put sym prop origdef)
		  (cc-bytecomp-debug-msg
		   "cc-bytecomp-restore-environment: Restored property %s for %s to %s"
		   prop sym origdef))
	      (cc-bytecomp-debug-msg
	       "cc-bytecomp-restore-environment: Not restoring property %s for %s"
	       prop sym)))
	  (setq p (cdr p)))
	(setq cc-bytecomp-environment-set nil)
	(cc-bytecomp-debug-msg
	 "cc-bytecomp-restore-environment: Done"))))

(eval
 ;; This eval is to avoid byte compilation of the function below.
 ;; There's some bug in XEmacs 21.4.6 that can cause it to dump core
 ;; here otherwise.  My theory is that `cc-bytecomp-load' might be
 ;; redefined recursively during the `load' inside it, and if it in
 ;; that case is byte compiled then the byte interpreter gets
 ;; confused.  I haven't succeeded in isolating the bug, though. /mast

 '(defun cc-bytecomp-load (cc-part)
    ;; Eval'ed during compilation to load a CC Mode file from the source
    ;; directory (assuming it's the same as the compiled file
    ;; destination dir).
    (if (and (boundp 'byte-compile-dest-file)
	     (stringp byte-compile-dest-file))
	(progn
	  (cc-bytecomp-restore-environment)
	  (let ((load-path
		 (cons (file-name-directory byte-compile-dest-file)
		       load-path))
		(cc-file (concat cc-part ".el")))
	    (if (member cc-file cc-bytecomp-loaded-files)
		()
	      (setq cc-bytecomp-loaded-files
		    (cons cc-file cc-bytecomp-loaded-files))
	      (cc-bytecomp-debug-msg
	       "cc-bytecomp-load: Loading %S" cc-file)
	      (load cc-file nil t t)
	      (cc-bytecomp-debug-msg
	       "cc-bytecomp-load: Loaded %S" cc-file)))
	  (cc-bytecomp-setup-environment)
	  t))))

(defmacro cc-require (cc-part)
  "Force loading of the corresponding .el file in the current directory
during compilation, but compile in a `require'.  Don't use within
`eval-when-compile'.

Having cyclic cc-require's will result in infinite recursion.  That's
somewhat intentional."
  `(progn
     (eval-when-compile (cc-bytecomp-load (symbol-name ,cc-part)))
     (require ,cc-part)))

(defmacro cc-provide (feature)
  "A replacement for the `provide' form that restores the environment
after the compilation.  Don't use within `eval-when-compile'."
  `(progn
     (eval-when-compile (cc-bytecomp-restore-environment))
     (provide ,feature)))

(defmacro cc-load (cc-part)
  "Force loading of the corresponding .el file in the current directory
during compilation.  Don't use outside `eval-when-compile' or
`eval-and-compile'.

Having cyclic cc-load's will result in infinite recursion.  That's
somewhat intentional."
  `(or (and (featurep 'cc-bytecomp)
	    (cc-bytecomp-load ,cc-part))
       (load ,cc-part nil t nil)))

(defmacro cc-require-when-compile (cc-part)
  "Force loading of the corresponding .el file in the current directory
during compilation, but do a compile time `require' otherwise.  Don't
use within `eval-when-compile'."
  `(eval-when-compile
     (if (and (featurep 'cc-bytecomp)
	      (cc-bytecomp-is-compiling))
	 (if (or (not load-in-progress)
		 (not (featurep ,cc-part)))
	     (cc-bytecomp-load (symbol-name ,cc-part)))
       (require ,cc-part))))

(defmacro cc-external-require (feature)
  "Do a `require' of an external package.
This restores and sets up the compilation environment before and
afterwards.  Don't use within `eval-when-compile'."
  `(progn
     (eval-when-compile (cc-bytecomp-restore-environment))
     (require ,feature)
     (eval-when-compile (cc-bytecomp-setup-environment))))

(defun cc-bytecomp-is-compiling ()
  "Return non-nil if eval'ed during compilation.  Don't use outside
`eval-when-compile'."
  (and (boundp 'byte-compile-dest-file)
       (stringp byte-compile-dest-file)))

(defmacro cc-bytecomp-defvar (var)
  "Binds the symbol as a variable during compilation of the file,
to silence the byte compiler.  Don't use within `eval-when-compile'."
  `(eval-when-compile
     (if (boundp ',var)
	 (cc-bytecomp-debug-msg
	  "cc-bytecomp-defvar: %s bound already as variable" ',var)
       (if (not (memq ',var cc-bytecomp-unbound-variables))
	   (progn
	     (cc-bytecomp-debug-msg
	      "cc-bytecomp-defvar: Saving %s (as unbound)" ',var)
	     (setq cc-bytecomp-unbound-variables
		   (cons ',var cc-bytecomp-unbound-variables))))
       (if (and (cc-bytecomp-is-compiling)
		(not load-in-progress))
	   (progn
	     (defvar ,var)
	     (set ',var (intern (concat "cc-bytecomp-ignore-var:"
					(symbol-name ',var))))
	     (cc-bytecomp-debug-msg
	      "cc-bytecomp-defvar: Covered variable %s" ',var))))))

(defmacro cc-bytecomp-defun (fun)
  "Bind the symbol as a function during compilation of the file,
to silence the byte compiler.  Don't use within `eval-when-compile'.

If the symbol already is bound as a function, it will keep that
definition.  That means that this macro will not shut up warnings
about incorrect number of arguments.  It's dangerous to try to replace
existing functions since the byte compiler might need the definition
at compile time, e.g. for macros and inline functions."
  `(eval-when-compile
     (if (fboundp ',fun)
	 (cc-bytecomp-debug-msg
	  "cc-bytecomp-defun: %s bound already as function" ',fun)
       (if (not (assq ',fun cc-bytecomp-original-functions))
	   (progn
	     (cc-bytecomp-debug-msg
	      "cc-bytecomp-defun: Saving %s (as unbound)" ',fun)
	     (setq cc-bytecomp-original-functions
		   (cons (list ',fun nil 'unbound)
			 cc-bytecomp-original-functions))))
       (if (and (cc-bytecomp-is-compiling)
		(not load-in-progress))
	   (progn
	     (fset ',fun (intern (concat "cc-bytecomp-ignore-fun:"
					 (symbol-name ',fun))))
	     (cc-bytecomp-debug-msg
	      "cc-bytecomp-defun: Covered function %s" ',fun))))))

(put 'cc-bytecomp-defmacro 'lisp-indent-function 'defun)
(defmacro cc-bytecomp-defmacro (fun &rest temp-macro)
  "Bind the symbol as a macro during compilation (and evaluation) of the
file.  Don't use outside `eval-when-compile'."
  `(let ((orig-fun (assq ',fun cc-bytecomp-original-functions)))
     (if (not orig-fun)
	 (setq orig-fun
	       (list ',fun
		     nil
		     (if (fboundp ',fun)
			 (progn
			   (cc-bytecomp-debug-msg
			    "cc-bytecomp-defmacro: Saving %s" ',fun)
			   (symbol-function ',fun))
		       (cc-bytecomp-debug-msg
			"cc-bytecomp-defmacro: Saving %s as unbound" ',fun)
		       'unbound))
	       cc-bytecomp-original-functions
	       (cons orig-fun cc-bytecomp-original-functions)))
     (defmacro ,fun ,@temp-macro)
     (cc-bytecomp-debug-msg
      "cc-bytecomp-defmacro: Bound macro %s" ',fun)
     (setcar (cdr orig-fun) (symbol-function ',fun))))

(defmacro cc-bytecomp-put (symbol propname value)
  "Set a property on a symbol during compilation (and evaluation) of
the file.  Don't use outside `eval-when-compile'."
  `(eval-when-compile
     (if (not (assoc (cons ,symbol ,propname) cc-bytecomp-original-properties))
	 (progn
	   (cc-bytecomp-debug-msg
	    "cc-bytecomp-put: Saving property %s for %s with value %s"
	    ,propname ,symbol (get ,symbol ,propname))
	   (setq cc-bytecomp-original-properties
		 (cons (cons (cons ,symbol ,propname)
			     (cons ,value (get ,symbol ,propname)))
		       cc-bytecomp-original-properties))))
     (put ,symbol ,propname ,value)
     (cc-bytecomp-debug-msg
      "cc-bytecomp-put: Bound property %s for %s to %s"
      ,propname ,symbol ,value)))

(defmacro cc-bytecomp-obsolete-var (symbol)
  "Suppress warnings that the given symbol is an obsolete variable.
Don't use within `eval-when-compile'."
  `(eval-when-compile
     (if (get ',symbol 'byte-obsolete-variable)
	 (cc-bytecomp-put ',symbol 'byte-obsolete-variable nil)
       ;; This avoids a superfluous compiler warning
       ;; about calling `get' for effect.
       t)))

(defun cc-bytecomp-ignore-obsolete (form)
  ;; Wraps a call to `byte-compile-obsolete' that suppresses the warning.
  (let ((byte-compile-warnings byte-compile-warnings))
    (byte-compile-disable-warning 'obsolete)
    (byte-compile-obsolete form)))

(defmacro cc-bytecomp-obsolete-fun (symbol)
  "Suppress warnings that the given symbol is an obsolete function.
Don't use within `eval-when-compile'."
  `(eval-when-compile
     (if (eq (get ',symbol 'byte-compile) 'byte-compile-obsolete)
	 (cc-bytecomp-put ',symbol 'byte-compile
			  'cc-bytecomp-ignore-obsolete)
       ;; This avoids a superfluous compiler warning
       ;; about calling `get' for effect.
       t)))

(defmacro cc-bytecomp-boundp (symbol)
  "Return non-nil if the given symbol is bound as a variable outside
the compilation.  This is the same as using `boundp' but additionally
exclude any variables that have been bound during compilation with
`cc-bytecomp-defvar'."
  (if (and (cc-bytecomp-is-compiling)
	   (memq (car (cdr symbol)) cc-bytecomp-unbound-variables))
      nil
    `(boundp ,symbol)))

(defmacro cc-bytecomp-fboundp (symbol)
  "Return non-nil if the given symbol is bound as a function outside
the compilation.  This is the same as using `fboundp' but additionally
exclude any functions that have been bound during compilation with
`cc-bytecomp-defun'."
  (let (fun-elem)
    (if (and (cc-bytecomp-is-compiling)
	     (setq fun-elem (assq (car (cdr symbol))
				  cc-bytecomp-original-functions))
	     (eq (elt fun-elem 2) 'unbound))
	nil
      `(fboundp ,symbol))))


(provide 'cc-bytecomp)

;;; cc-bytecomp.el ends here
