;;; cl.el --- Common Lisp extensions for Emacs

;; Copyright (C) 1993, 2001-2012  Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
;; Keywords: extensions

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

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the portions of the Common Lisp extensions
;; package which should always be present.


;;; Future notes:

;; Once Emacs 19 becomes standard, many things in this package which are
;; messy for reasons of compatibility can be greatly simplified.  For now,
;; I prefer to maintain one unified version.


;;; Change Log:

;; Version 2.02 (30 Jul 93):
;;  * Added "cl-compat.el" file, extra compatibility with old package.
;;  * Added `lexical-let' and `lexical-let*'.
;;  * Added `define-modify-macro', `callf', and `callf2'.
;;  * Added `ignore-errors'.
;;  * Changed `(setf (nthcdr N PLACE) X)' to work when N is zero.
;;  * Merged `*gentemp-counter*' into `*gensym-counter*'.
;;  * Extended `subseq' to allow negative START and END like `substring'.
;;  * Added `in-ref', `across-ref', `elements of-ref' loop clauses.
;;  * Added `concat', `vconcat' loop clauses.
;;  * Cleaned up a number of compiler warnings.

;; Version 2.01 (7 Jul 93):
;;  * Added support for FSF version of Emacs 19.
;;  * Added `add-hook' for Emacs 18 users.
;;  * Added `defsubst*' and `symbol-macrolet'.
;;  * Added `maplist', `mapc', `mapl', `mapcan', `mapcon'.
;;  * Added `map', `concatenate', `reduce', `merge'.
;;  * Added `revappend', `nreconc', `tailp', `tree-equal'.
;;  * Added `assert', `check-type', `typecase', `typep', and `deftype'.
;;  * Added destructuring and `&environment' support to `defmacro*'.
;;  * Added destructuring to `loop', and added the following clauses:
;;      `elements', `frames', `overlays', `intervals', `buffers', `key-seqs'.
;;  * Renamed `delete' to `delete*' and `remove' to `remove*'.
;;  * Completed support for all keywords in `remove*', `substitute', etc.
;;  * Added `most-positive-float' and company.
;;  * Fixed hash tables to work with latest Lucid Emacs.
;;  * `proclaim' forms are no longer compile-time-evaluating; use `declaim'.
;;  * Syntax for `warn' declarations has changed.
;;  * Improved implementation of `random*'.
;;  * Moved most sequence functions to a new file, cl-seq.el.
;;  * Moved `eval-when' into cl-macs.el.
;;  * Moved `pushnew' and `adjoin' to cl.el for most common cases.
;;  * Moved `provide' forms down to ends of files.
;;  * Changed expansion of `pop' to something that compiles to better code.
;;  * Changed so that no patch is required for Emacs 19 byte compiler.
;;  * Made more things dependent on `optimize' declarations.
;;  * Added a partial implementation of struct print functions.
;;  * Miscellaneous minor changes.

;; Version 2.00:
;;  * First public release of this package.


;;; Code:

(defvar cl-optimize-speed 1)
(defvar cl-optimize-safety 1)


;;;###autoload
(defvar custom-print-functions nil
  "This is a list of functions that format user objects for printing.
Each function is called in turn with three arguments: the object, the
stream, and the print level (currently ignored).  If it is able to
print the object it returns true; otherwise it returns nil and the
printer proceeds to the next function on the list.

This variable is not used at present, but it is defined in hopes that
a future Emacs interpreter will be able to use it.")

(defun cl-unload-function ()
  "Stop unloading of the Common Lisp extensions."
  (message "Cannot unload the feature `cl'")
  ;; stop standard unloading!
  t)

;;; Generalized variables.
;; These macros are defined here so that they
;; can safely be used in .emacs files.

(defmacro incf (place &optional x)
  "Increment PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the incremented value of PLACE."
  (if (symbolp place)
      (list 'setq place (if x (list '+ place x) (list '1+ place)))
    (list 'callf '+ place (or x 1))))

(defmacro decf (place &optional x)
  "Decrement PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the decremented value of PLACE."
  (if (symbolp place)
      (list 'setq place (if x (list '- place x) (list '1- place)))
    (list 'callf '- place (or x 1))))

;; Autoloaded, but we haven't loaded cl-loaddefs yet.
(declare-function cl-do-pop "cl-macs" (place))

(defmacro pop (place)
  "Remove and return the head of the list stored in PLACE.
Analogous to (prog1 (car PLACE) (setf PLACE (cdr PLACE))), though more
careful about evaluating each argument only once and in the right order.
PLACE may be a symbol, or any generalized variable allowed by `setf'."
  (if (symbolp place)
      (list 'car (list 'prog1 place (list 'setq place (list 'cdr place))))
    (cl-do-pop place)))

(defmacro push (x place)
  "Insert X at the head of the list stored in PLACE.
Analogous to (setf PLACE (cons X PLACE)), though more careful about
evaluating each argument only once and in the right order.  PLACE may
be a symbol, or any generalized variable allowed by `setf'."
  (if (symbolp place) (list 'setq place (list 'cons x place))
    (list 'callf2 'cons x place)))

(defmacro pushnew (x place &rest keys)
  "(pushnew X PLACE): insert X at the head of the list if not already there.
Like (push X PLACE), except that the list is unmodified if X is `eql' to
an element already on the list.
\nKeywords supported:  :test :test-not :key
\n(fn X PLACE [KEYWORD VALUE]...)"
  (if (symbolp place)
      (if (null keys)
 	  `(let ((x ,x))
	     (if (memql x ,place)
                 ;; This symbol may later on expand to actual code which then
                 ;; trigger warnings like "value unused" since pushnew's return
                 ;; value is rarely used.  It should not matter that other
                 ;; warnings may be silenced, since `place' is used earlier and
                 ;; should have triggered them already.
                 (with-no-warnings ,place)
               (setq ,place (cons x ,place))))
	(list 'setq place (list* 'adjoin x place keys)))
    (list* 'callf2 'adjoin x place keys)))

(defun cl-set-elt (seq n val)
  (if (listp seq) (setcar (nthcdr n seq) val) (aset seq n val)))

(defsubst cl-set-nthcdr (n list x)
  (if (<= n 0) x (setcdr (nthcdr (1- n) list) x) list))

(defun cl-set-buffer-substring (start end val)
  (save-excursion (delete-region start end)
		  (goto-char start)
		  (insert val)
		  val))

(defun cl-set-substring (str start end val)
  (if end (if (< end 0) (incf end (length str)))
    (setq end (length str)))
  (if (< start 0) (incf start (length str)))
  (concat (and (> start 0) (substring str 0 start))
	  val
	  (and (< end (length str)) (substring str end))))


;;; Control structures.

;; These macros are so simple and so often-used that it's better to have
;; them all the time than to load them from cl-macs.el.

(defun cl-map-extents (&rest cl-args)
  (apply 'cl-map-overlays cl-args))


;;; Blocks and exits.

(defalias 'cl-block-wrapper 'identity)
(defalias 'cl-block-throw 'throw)


;;; Multiple values.
;; True multiple values are not supported, or even
;; simulated.  Instead, multiple-value-bind and friends simply expect
;; the target form to return the values as a list.

(defsubst values (&rest values)
  "Return multiple values, Common Lisp style.
The arguments of `values' are the values
that the containing function should return."
  values)

(defsubst values-list (list)
  "Return multiple values, Common Lisp style, taken from a list.
LIST specifies the list of values
that the containing function should return."
  list)

(defsubst multiple-value-list (expression)
  "Return a list of the multiple values produced by EXPRESSION.
This handles multiple values in Common Lisp style, but it does not
work right when EXPRESSION calls an ordinary Emacs Lisp function
that returns just one value."
  expression)

(defsubst multiple-value-apply (function expression)
  "Evaluate EXPRESSION to get multiple values and apply FUNCTION to them.
This handles multiple values in Common Lisp style, but it does not work
right when EXPRESSION calls an ordinary Emacs Lisp function that returns just
one value."
  (apply function expression))

(defalias 'multiple-value-call 'apply
  "Apply FUNCTION to ARGUMENTS, taking multiple values into account.
This implementation only handles the case where there is only one argument.")

(defsubst nth-value (n expression)
  "Evaluate EXPRESSION to get multiple values and return the Nth one.
This handles multiple values in Common Lisp style, but it does not work
right when EXPRESSION calls an ordinary Emacs Lisp function that returns just
one value."
  (nth n expression))

;;; Macros.

(defvar cl-macro-environment)
(defvar cl-old-macroexpand (prog1 (symbol-function 'macroexpand)
			     (defalias 'macroexpand 'cl-macroexpand)))

(defun cl-macroexpand (cl-macro &optional cl-env)
  "Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.
\n(fn FORM &optional ENVIRONMENT)"
  (let ((cl-macro-environment cl-env))
    (while (progn (setq cl-macro (funcall cl-old-macroexpand cl-macro cl-env))
		  (and (symbolp cl-macro)
		       (cdr (assq (symbol-name cl-macro) cl-env))))
      (setq cl-macro (cadr (assq (symbol-name cl-macro) cl-env))))
    cl-macro))


;;; Declarations.

(defvar cl-compiling-file nil)
(defun cl-compiling-file ()
  (or cl-compiling-file
      (and (boundp 'byte-compile--outbuffer)
           (bufferp (symbol-value 'byte-compile--outbuffer))
	   (equal (buffer-name (symbol-value 'byte-compile--outbuffer))
		  " *Compiler Output*"))))

(defvar cl-proclaims-deferred nil)

(defun proclaim (spec)
  (if (fboundp 'cl-do-proclaim) (cl-do-proclaim spec t)
    (push spec cl-proclaims-deferred))
  nil)

(defmacro declaim (&rest specs)
  (let ((body (mapcar (function (lambda (x) (list 'proclaim (list 'quote x))))
		      specs)))
    (if (cl-compiling-file) (list* 'eval-when '(compile load eval) body)
      (cons 'progn body))))   ; avoid loading cl-macs.el for eval-when


;;; Symbols.

(defun cl-random-time ()
  (let* ((time (copy-sequence (current-time-string))) (i (length time)) (v 0))
    (while (>= (decf i) 0) (setq v (+ (* v 3) (aref time i))))
    v))

(defvar *gensym-counter* (* (logand (cl-random-time) 1023) 100))


;;; Numbers.

(defun floatp-safe (object)
  "Return t if OBJECT is a floating point number.
On Emacs versions that lack floating-point support, this function
always returns nil."
  (and (numberp object) (not (integerp object))))

(defun plusp (number)
  "Return t if NUMBER is positive."
  (> number 0))

(defun minusp (number)
  "Return t if NUMBER is negative."
  (< number 0))

(defun oddp (integer)
  "Return t if INTEGER is odd."
  (eq (logand integer 1) 1))

(defun evenp (integer)
  "Return t if INTEGER is even."
  (eq (logand integer 1) 0))

(defvar *random-state* (vector 'cl-random-state-tag -1 30 (cl-random-time)))

(defconst most-positive-float nil
  "The largest value that a Lisp float can hold.
If your system supports infinities, this is the largest finite value.
For IEEE machines, this is approximately 1.79e+308.
Call `cl-float-limits' to set this.")

(defconst most-negative-float nil
  "The largest negative value that a Lisp float can hold.
This is simply -`most-positive-float'.
Call `cl-float-limits' to set this.")

(defconst least-positive-float nil
  "The smallest value greater than zero that a Lisp float can hold.
For IEEE machines, it is about 4.94e-324 if denormals are supported,
or 2.22e-308 if they are not.
Call `cl-float-limits' to set this.")

(defconst least-negative-float nil
  "The smallest value less than zero that a Lisp float can hold.
This is simply -`least-positive-float'.
Call `cl-float-limits' to set this.")

(defconst least-positive-normalized-float nil
  "The smallest normalized Lisp float greater than zero.
This is the smallest value for which IEEE denormalization does not lose
precision.  For IEEE machines, this value is about 2.22e-308.
For machines that do not support the concept of denormalization
and gradual underflow, this constant equals `least-positive-float'.
Call `cl-float-limits' to set this.")

(defconst least-negative-normalized-float nil
  "The smallest normalized Lisp float less than zero.
This is simply -`least-positive-normalized-float'.
Call `cl-float-limits' to set this.")

(defconst float-epsilon nil
  "The smallest positive float that adds to 1.0 to give a distinct value.
Adding a number less than this to 1.0 returns 1.0 due to roundoff.
For IEEE machines, epsilon is about 2.22e-16.
Call `cl-float-limits' to set this.")

(defconst float-negative-epsilon nil
  "The smallest positive float that subtracts from 1.0 to give a distinct value.
For IEEE machines, it is about 1.11e-16.
Call `cl-float-limits' to set this.")


;;; Sequence functions.

(defalias 'copy-seq 'copy-sequence)

(declare-function cl-mapcar-many "cl-extra" (cl-func cl-seqs))

(defun mapcar* (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)"
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
	  (cl-mapcar-many cl-func (cons cl-x cl-rest))
	(let ((cl-res nil) (cl-y (car cl-rest)))
	  (while (and cl-x cl-y)
	    (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
	  (nreverse cl-res)))
    (mapcar cl-func cl-x)))

(defalias 'svref 'aref)

;;; List functions.

(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'rest 'cdr)
(defalias 'endp 'null)

(defun third (x)
  "Return the third element of the list X."
  (car (cdr (cdr x))))

(defun fourth (x)
  "Return the fourth element of the list X."
  (nth 3 x))

(defun fifth (x)
  "Return the fifth element of the list X."
  (nth 4 x))

(defun sixth (x)
  "Return the sixth element of the list X."
  (nth 5 x))

(defun seventh (x)
  "Return the seventh element of the list X."
  (nth 6 x))

(defun eighth (x)
  "Return the eighth element of the list X."
  (nth 7 x))

(defun ninth (x)
  "Return the ninth element of the list X."
  (nth 8 x))

(defun tenth (x)
  "Return the tenth element of the list X."
  (nth 9 x))

(defun caaar (x)
  "Return the `car' of the `car' of the `car' of X."
  (car (car (car x))))

(defun caadr (x)
  "Return the `car' of the `car' of the `cdr' of X."
  (car (car (cdr x))))

(defun cadar (x)
  "Return the `car' of the `cdr' of the `car' of X."
  (car (cdr (car x))))

(defun caddr (x)
  "Return the `car' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr x))))

(defun cdaar (x)
  "Return the `cdr' of the `car' of the `car' of X."
  (cdr (car (car x))))

(defun cdadr (x)
  "Return the `cdr' of the `car' of the `cdr' of X."
  (cdr (car (cdr x))))

(defun cddar (x)
  "Return the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (car x))))

(defun cdddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr x))))

(defun caaaar (x)
  "Return the `car' of the `car' of the `car' of the `car' of X."
  (car (car (car (car x)))))

(defun caaadr (x)
  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (car (car (car (cdr x)))))

(defun caadar (x)
  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (car (car (cdr (car x)))))

(defun caaddr (x)
  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (car (car (cdr (cdr x)))))

(defun cadaar (x)
  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (car (cdr (car (car x)))))

(defun cadadr (x)
  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (car (cdr (car (cdr x)))))

(defun caddar (x)
  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (car (cdr (cdr (car x)))))

(defun cadddr (x)
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr (cdr x)))))

(defun cdaaar (x)
  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (cdr (car (car (car x)))))

(defun cdaadr (x)
  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (cdr (car (car (cdr x)))))

(defun cdadar (x)
  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (cdr (car (cdr (car x)))))

(defun cdaddr (x)
  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (cdr (car (cdr (cdr x)))))

(defun cddaar (x)
  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (cdr (cdr (car (car x)))))

(defun cddadr (x)
  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (cdr (cdr (car (cdr x)))))

(defun cdddar (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (cdr (car x)))))

(defun cddddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr (cdr x)))))

;;(defun last* (x &optional n)
;;  "Returns the last link in the list LIST.
;;With optional argument N, returns Nth-to-last link (default 1)."
;;  (if n
;;      (let ((m 0) (p x))
;;	(while (consp p) (incf m) (pop p))
;;	(if (<= n 0) p
;;	  (if (< n m) (nthcdr (- m n) x) x)))
;;    (while (consp (cdr x)) (pop x))
;;    x))

(defun list* (arg &rest rest)   ; See compiler macro in cl-macs.el
  "Return a new list with specified ARGs as elements, consed to last ARG.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'.
\n(fn ARG...)"
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

(defun ldiff (list sublist)
  "Return a copy of LIST with the tail SUBLIST removed."
  (let ((res nil))
    (while (and (consp list) (not (eq list sublist)))
      (push (pop list) res))
    (nreverse res)))

(defun copy-list (list)
  "Return a copy of LIST, which may be a dotted list.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))

(defun cl-maclisp-member (item list)
  (while (and list (not (equal item (car list)))) (setq list (cdr list)))
  list)

(defalias 'cl-member 'memq)   ; for compatibility with old CL package

;; Autoloaded, but we have not loaded cl-loaddefs yet.
(declare-function floor* "cl-extra" (x &optional y))
(declare-function ceiling* "cl-extra" (x &optional y))
(declare-function truncate* "cl-extra" (x &optional y))
(declare-function round* "cl-extra" (x &optional y))
(declare-function mod* "cl-extra" (x y))

(defalias 'cl-floor 'floor*)
(defalias 'cl-ceiling 'ceiling*)
(defalias 'cl-truncate 'truncate*)
(defalias 'cl-round 'round*)
(defalias 'cl-mod 'mod*)

(defun adjoin (cl-item cl-list &rest cl-keys)  ; See compiler macro in cl-macs
  "Return ITEM consed onto the front of LIST only if it's not already there.
Otherwise, return LIST unmodified.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (cond ((or (equal cl-keys '(:test eq))
	     (and (null cl-keys) (not (numberp cl-item))))
	 (if (memq cl-item cl-list) cl-list (cons cl-item cl-list)))
	((or (equal cl-keys '(:test equal)) (null cl-keys))
	 (if (member cl-item cl-list) cl-list (cons cl-item cl-list)))
	(t (apply 'cl-adjoin cl-item cl-list cl-keys))))

(defun subst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (non-destructively).
Return a copy of TREE with all elements `eql' to OLD replaced by NEW.
\nKeywords supported:  :test :test-not :key
\n(fn NEW OLD TREE [KEYWORD VALUE]...)"
  (if (or cl-keys (and (numberp cl-old) (not (integerp cl-old))))
      (apply 'sublis (list (cons cl-old cl-new)) cl-tree cl-keys)
    (cl-do-subst cl-new cl-old cl-tree)))

(defun cl-do-subst (cl-new cl-old cl-tree)
  (cond ((eq cl-tree cl-old) cl-new)
	((consp cl-tree)
	 (let ((a (cl-do-subst cl-new cl-old (car cl-tree)))
	       (d (cl-do-subst cl-new cl-old (cdr cl-tree))))
	   (if (and (eq a (car cl-tree)) (eq d (cdr cl-tree)))
	       cl-tree (cons a d))))
	(t cl-tree)))

(defun acons (key value alist)
  "Add KEY and VALUE to ALIST.
Return a new list with (cons KEY VALUE) as car and ALIST as cdr."
  (cons (cons key value) alist))

(defun pairlis (keys values &optional alist)
  "Make an alist from KEYS and VALUES.
Return a new alist composed by associating KEYS to corresponding VALUES;
the process stops as soon as KEYS or VALUES run out.
If ALIST is non-nil, the new pairs are prepended to it."
  (nconc (mapcar* 'cons keys values) alist))


;;; Miscellaneous.

;; Define data for indentation and edebug.
(dolist (entry
         '(((defun* defmacro*) 2)
           ((function*) nil
            (&or symbolp ([&optional 'macro] 'lambda (&rest sexp) &rest form)))
           ((eval-when) 1 (sexp &rest form))
           ((declare) nil (&rest sexp))
           ((the) 1 (sexp &rest form))
           ((case ecase typecase etypecase) 1 (form &rest (sexp &rest form)))
           ((block return-from) 1 (sexp &rest form))
           ((return) nil (&optional form))
           ((do do*) 2 ((&rest &or symbolp (symbolp &optional form form))
                        (form &rest form)
                        &rest form))
           ((do-symbols) 1 ((symbolp form &optional form form) &rest form))
           ((do-all-symbols) 1 ((symbolp form &optional form) &rest form))
           ((psetq setf psetf) nil edebug-setq-form)
           ((progv) 2 (&rest form))
           ((flet labels macrolet) 1
            ((&rest (sexp sexp &rest form)) &rest form))
           ((symbol-macrolet lexical-let lexical-let*) 1
            ((&rest &or symbolp (symbolp form)) &rest form))
           ((multiple-value-bind) 2 ((&rest symbolp) &rest form))
           ((multiple-value-setq) 1 ((&rest symbolp) &rest form))
           ((incf decf remf pushnew shiftf rotatef) nil (&rest form))
           ((letf letf*) 1 ((&rest (&rest form)) &rest form))
           ((callf destructuring-bind) 2 (sexp form &rest form))
           ((callf2) 3 (sexp form form &rest form))
           ((loop) nil (&rest &or symbolp form))
           ((ignore-errors) 0 (&rest form))))
  (dolist (func (car entry))
    (put func 'lisp-indent-function (nth 1 entry))
    (put func 'lisp-indent-hook (nth 1 entry))
    (or (get func 'edebug-form-spec)
        (put func 'edebug-form-spec (nth 2 entry)))))

;; Autoload the other portions of the package.
;; We want to replace the basic versions of dolist, dotimes, declare below.
(fmakunbound 'dolist)
(fmakunbound 'dotimes)
(fmakunbound 'declare)
(load "cl-loaddefs" nil 'quiet)

;; This goes here so that cl-macs can find it if it loads right now.
(provide 'cl)

;; Things to do after byte-compiler is loaded.

(defvar cl-hacked-flag nil)
(defun cl-hack-byte-compiler ()
  (and (not cl-hacked-flag) (fboundp 'byte-compile-file-form)
       (progn
         (setq cl-hacked-flag t)  ; Do it first, to prevent recursion.
         (load "cl-macs" nil t)
         (run-hooks 'cl-hack-bytecomp-hook))))

;; Try it now in case the compiler has already been loaded.
(cl-hack-byte-compiler)

;; Also make a hook in case compiler is loaded after this file.
(add-hook 'bytecomp-load-hook 'cl-hack-byte-compiler)


;; The following ensures that packages which expect the old-style cl.el
;; will be happy with this one.

(provide 'cl)

(run-hooks 'cl-load-hook)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; cl.el ends here
