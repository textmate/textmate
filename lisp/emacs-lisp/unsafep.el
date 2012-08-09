;;;; unsafep.el -- Determine whether a Lisp form is safe to evaluate

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Author: Jonathan Yavner <jyavner@member.fsf.org>
;; Maintainer: Jonathan Yavner <jyavner@member.fsf.org>
;; Keywords: safety lisp utility

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

;; This is a simplistic implementation that does not allow any modification of
;; buffers or global variables.  It does no dataflow analysis, so functions
;; like `funcall' and `setcar' are completely disallowed.  It is designed
;; for "pure Lisp" formulas, like those in spreadsheets, that don't make any
;; use of the text editing capabilities of Emacs.

;; A formula is safe if:
;;  1.  It's an atom.
;;  2.  It's a function call to a safe function and all arguments are safe
;;      formulas.
;;  3.  It's a special form whose arguments are like a function's (and,
;;	catch, if, or, prog1, prog2, progn, while, unwind-protect).
;;  4.  It's a special form or macro that creates safe temporary bindings
;;      (condition-case, dolist, dotimes, lambda, let, let*).
;;  4.  It's one of (cond, quote) that have special parsing.
;;  5.  It's one of (add-to-list, setq, push, pop) and the assignment variable
;;      is safe.
;;  6.  It's one of (apply, mapc, mapcar, mapconcat) and its first arg is a
;;      quoted safe function.
;;
;; A function is safe if:
;;  1.  It's a lambda containing safe formulas.
;;  2.  It's a member of list `safe-functions', so the user says it's safe.
;;  3.  It's a symbol with the `side-effect-free' property, defined by the
;;      byte compiler or function author.
;;  4.  It's a symbol with the `safe-function' property, defined here or by
;;      the function author.  Value t indicates a function that is safe but
;;      has innocuous side effects.  Other values will someday indicate
;;      functions with side effects that are not always safe.
;;  The `side-effect-free' and `safe-function' properties are provided for
;;  built-in functions and for functions and macros defined in subr.el.
;;
;; A temporary binding is unsafe if its symbol:
;;  1.  Has the `risky-local-variable' property.
;;  2.  Has a name that ends with -command, font-lock-keywords(-[0-9]+)?,
;;      font-lock-syntactic-keywords, -form, -forms, -frame-alist, -function,
;;       -functions, -history, -hook, -hooks, -map, -map-alist, -mode-alist,
;;       -predicate, or -program.
;;
;; An assignment variable is unsafe if:
;;   1. It would be unsafe as a temporary binding.
;;   2. It doesn't already have a temporary or buffer-local binding.

;; There are unsafe forms that `unsafep' cannot detect.  Beware of these:
;;   1. The form's result is a string with a display property containing a
;;      form to be evaluated later, and you insert this result into a
;;      buffer.  Always remove display properties before inserting!
;;   2. The form alters a risky variable that was recently added to Emacs and
;;      is not yet marked with the `risky-local-variable' property.
;;   3. The form uses undocumented features of built-in functions that have
;;      the `side-effect-free' property.  For example, in Emacs-20 if you
;;      passed a circular list to `assoc', Emacs would crash.  Historically,
;;      problems of this kind have been few and short-lived.

;;; Code:

(provide 'unsafep)
(require 'byte-opt)  ;Set up the `side-effect-free' properties

(defcustom safe-functions nil
  "A list of assumed-safe functions, or t to disable `unsafep'."
  :group 'lisp
  :type  '(choice (const :tag "No" nil) (const :tag "Yes" t) hook))

(defvar unsafep-vars nil
  "Dynamically-bound list of variables with lexical bindings at this point
in the parse.")
(put 'unsafep-vars 'risky-local-variable t)

;;Side-effect-free functions from subr.el
(dolist (x '(assoc-default assoc-ignore-case butlast last match-string
	     match-string-no-properties member-ignore-case remove remq))
  (put x 'side-effect-free t))

;;Other safe functions
(dolist (x '(;;Special forms
	     and catch if or prog1 prog2 progn while unwind-protect
	     ;;Safe subrs that have some side-effects
	     ding error random signal sleep-for string-match throw
	     ;;Defsubst functions from subr.el
	     caar cadr cdar cddr
	     ;;Macros from subr.el
	     save-match-data unless when
	     ;;Functions from subr.el that have side effects
	     split-string replace-regexp-in-string play-sound-file))
  (put x 'safe-function t))

;;;###autoload
(defun unsafep (form &optional unsafep-vars)
  "Return nil if evaluating FORM couldn't possibly do any harm.
Otherwise result is a reason why FORM is unsafe.
UNSAFEP-VARS is a list of symbols with local bindings."
  (catch 'unsafep
    (if (or (eq safe-functions t)	    ;User turned off safety-checking
	    (atom form))		    ;Atoms are never unsafe
	(throw 'unsafep nil))
    (let* ((fun    (car form))
	   (reason (unsafep-function fun))
	   arg)
      (cond
       ((not reason)
	;;It's a normal function - unsafe if any arg is
	(unsafep-progn (cdr form)))
       ((eq fun 'quote)
	;;Never unsafe
	nil)
       ((memq fun '(apply mapc mapcar mapconcat))
	;;Unsafe if 1st arg isn't a quoted lambda
	(setq arg (cadr form))
	(cond
	 ((memq (car-safe arg) '(quote function))
	  (setq reason (unsafep-function (cadr arg))))
	 ((eq (car-safe arg) 'lambda)
	  ;;Self-quoting lambda
	  (setq reason (unsafep arg unsafep-vars)))
	 (t
	  (setq reason `(unquoted ,arg))))
	(or reason (unsafep-progn (cddr form))))
       ((eq fun 'lambda)
	;;First arg is temporary bindings
	(mapc #'(lambda (x)
		  (or (memq x '(&optional &rest))
		      (let ((y (unsafep-variable x t)))
			(if y (throw 'unsafep y))
			(push x unsafep-vars))))
	      (cadr form))
	(unsafep-progn (cddr form)))
       ((eq fun 'let)
	;;Creates temporary bindings in one step
	(setq unsafep-vars (nconc (mapcar #'unsafep-let (cadr form))
				  unsafep-vars))
	(unsafep-progn (cddr form)))
       ((eq fun 'let*)
	;;Creates temporary bindings iteratively
	(dolist (x (cadr form))
	  (push (unsafep-let x) unsafep-vars))
	(unsafep-progn (cddr form)))
       ((eq fun 'setq)
	;;Safe if odd arguments are local-var syms, evens are safe exprs
	(setq arg (cdr form))
	(while arg
	  (setq reason (or (unsafep-variable (car arg) nil)
			   (unsafep (cadr arg) unsafep-vars)))
	  (if reason (throw 'unsafep reason))
	  (setq arg (cddr arg))))
       ((eq fun 'pop)
	;;safe if arg is local-var sym
	(unsafep-variable (cadr form) nil))
       ((eq fun 'push)
	;;Safe if 2nd arg is a local-var sym
	(or (unsafep (cadr form) unsafep-vars)
	    (unsafep-variable (nth 2 form) nil)))
       ((eq fun 'add-to-list)
	;;Safe if first arg is a quoted local-var sym
	(setq arg (cadr form))
	(if (not (eq (car-safe arg) 'quote))
	    `(unquoted ,arg)
	  (or (unsafep-variable (cadr arg) nil)
	      (unsafep-progn (cddr form)))))
       ((eq fun 'cond)
	;;Special form with unusual syntax - safe if all args are
	(dolist (x (cdr form))
	  (setq reason (unsafep-progn x))
	  (if reason (throw 'unsafep reason))))
       ((memq fun '(dolist dotimes))
	;;Safe if COUNT and RESULT are safe.  VAR is bound while checking BODY.
	(setq arg (cadr form))
	(or (unsafep-progn (cdr arg))
	    (let ((unsafep-vars (cons (car arg) unsafep-vars)))
	      (unsafep-progn (cddr form)))))
       ((eq fun 'condition-case)
	;;Special form with unusual syntax - safe if all args are
	(or (unsafep-variable (cadr form) t)
	    (unsafep (nth 2 form) unsafep-vars)
	    (let ((unsafep-vars (cons (cadr form) unsafep-vars)))
	      ;;var is bound only during handlers
	      (dolist (x (nthcdr 3 form))
		(setq reason (unsafep-progn (cdr x)))
		(if reason (throw 'unsafep reason))))))
       ((eq fun '\`)
	;; Backquoted form - safe if its expansion is.
	(unsafep (cdr (backquote-process (cadr form)))))
       (t
	;;First unsafep-function call above wasn't nil, no special case applies
	reason)))))


(defun unsafep-function (fun)
  "Return nil if FUN is a safe function.
\(Either a safe lambda or a symbol that names a safe function).
Otherwise result is a reason code."
  (cond
   ((eq (car-safe fun) 'lambda)
    (unsafep fun unsafep-vars))
   ((not (and (symbolp fun)
	      (or (get fun 'side-effect-free)
		  (eq (get fun 'safe-function) t)
		  (eq safe-functions t)
		  (memq fun safe-functions))))
    `(function ,fun))))

(defun unsafep-progn (list)
  "Return nil if all forms in LIST are safe.
Else, return the reason for the first unsafe form."
  (catch 'unsafep-progn
    (let (reason)
      (dolist (x list)
	(setq reason (unsafep x unsafep-vars))
	(if reason (throw 'unsafep-progn reason))))))

(defun unsafep-let (clause)
  "Check the safety of a let binding.
CLAUSE is a let-binding, either SYM or (SYM) or (SYM VAL).
Check VAL and throw a reason to `unsafep' if unsafe.
Return SYM."
  (let (reason sym)
    (if (atom clause)
	(setq sym clause)
      (setq sym    (car clause)
	    reason (unsafep (cadr clause) unsafep-vars)))
    (setq reason (or (unsafep-variable sym t) reason))
    (if reason (throw 'unsafep reason))
    sym))

(defun unsafep-variable (sym to-bind)
  "Return nil if SYM is safe to set or bind, or a reason why not.
If TO-BIND is nil, check whether SYM is safe to set.
If TO-BIND is t, check whether SYM is safe to bind."
  (cond
   ((not (symbolp sym))
    `(variable ,sym))
   ((risky-local-variable-p sym nil)
    `(risky-local-variable ,sym))
   ((not (or to-bind
	     (memq sym unsafep-vars)
	     (local-variable-p sym)))
    `(global-variable ,sym))))

;;; unsafep.el ends here
