;;; macroexp.el --- Additional macro-expansion support -*- lexical-binding: t -*-
;;
;; Copyright (C) 2004-2012 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: lisp, compiler, macros

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
;; This file contains macro-expansions functions that are not defined in
;; the Lisp core, namely `macroexpand-all', which expands all macros in
;; a form, not just a top-level one.
;;

;;; Code:

(eval-when-compile (require 'cl))

;; Bound by the top-level `macroexpand-all', and modified to include any
;; macros defined by `defmacro'.
(defvar macroexpand-all-environment nil)

(defun maybe-cons (car cdr original-cons)
  "Return (CAR . CDR), using ORIGINAL-CONS if possible."
  (if (and (eq car (car original-cons)) (eq cdr (cdr original-cons)))
      original-cons
    (cons car cdr)))

;; We use this special macro to iteratively process forms and share list
;; structure of the result with the input.  Doing so recursively using
;; `maybe-cons' results in excessively deep recursion for very long
;; input forms.
(defmacro macroexp-accumulate (var+list &rest body)
  "Return a list of the results of evaluating BODY for each element of LIST.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Return a list of the values of the final form in BODY.
The list structure of the result will share as much with LIST as
possible (for instance, when BODY just returns VAR unchanged, the
result will be eq to LIST).

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  (let ((var (car var+list))
	(list (cadr var+list))
	(shared (make-symbol "shared"))
	(unshared (make-symbol "unshared"))
	(tail (make-symbol "tail"))
	(new-el (make-symbol "new-el")))
    `(let* ((,shared ,list)
	    (,unshared nil)
	    (,tail ,shared)
	    ,var ,new-el)
       (while ,tail
	 (setq ,var (car ,tail)
	       ,new-el (progn ,@body))
	 (unless (eq ,var ,new-el)
	   (while (not (eq ,shared ,tail))
	     (push (pop ,shared) ,unshared))
	   (setq ,shared (cdr ,shared))
	   (push ,new-el ,unshared))
	 (setq ,tail (cdr ,tail)))
       (nconc (nreverse ,unshared) ,shared))))

(defun macroexpand-all-forms (forms &optional skip)
  "Return FORMS with macros expanded.  FORMS is a list of forms.
If SKIP is non-nil, then don't expand that many elements at the start of
FORMS."
  (macroexp-accumulate (form forms)
    (if (or (null skip) (zerop skip))
	(macroexpand-all-1 form)
      (setq skip (1- skip))
      form)))

(defun macroexpand-all-clauses (clauses &optional skip)
  "Return CLAUSES with macros expanded.
CLAUSES is a list of lists of forms; any clause that's not a list is ignored.
If SKIP is non-nil, then don't expand that many elements at the start of
each clause."
  (macroexp-accumulate (clause clauses)
    (if (listp clause)
	(macroexpand-all-forms clause skip)
      clause)))

(defun macroexpand-all-1 (form)
  "Expand all macros in FORM.
This is an internal version of `macroexpand-all'.
Assumes the caller has bound `macroexpand-all-environment'."
  (if (and (listp form) (eq (car form) 'backquote-list*))
      ;; Special-case `backquote-list*', as it is normally a macro that
      ;; generates exceedingly deep expansions from relatively shallow input
      ;; forms.  We just process it `in reverse' -- first we expand all the
      ;; arguments, _then_ we expand the top-level definition.
      (macroexpand (macroexpand-all-forms form 1)
		   macroexpand-all-environment)
    ;; Normal form; get its expansion, and then expand arguments.
    (let ((new-form (macroexpand form macroexpand-all-environment)))
      (when (and (not (eq form new-form)) ;It was a macro call.
                 (car-safe form)
                 (symbolp (car form))
                 (get (car form) 'byte-obsolete-info)
                 (fboundp 'byte-compile-warn-obsolete))
        (byte-compile-warn-obsolete (car form)))
      (setq form new-form))
    (pcase form
      (`(cond . ,clauses)
       (maybe-cons 'cond (macroexpand-all-clauses clauses) form))
      (`(condition-case . ,(or `(,err ,body . ,handlers) dontcare))
       (maybe-cons
        'condition-case
        (maybe-cons err
                    (maybe-cons (macroexpand-all-1 body)
                                (macroexpand-all-clauses handlers 1)
                                (cddr form))
                    (cdr form))
        form))
      (`(defmacro ,name . ,args-and-body)
       (push (cons name (cons 'lambda args-and-body))
             macroexpand-all-environment)
       (let ((n 3))
         ;; Don't macroexpand `declare' since it should really be "expanded"
         ;; away when `defmacro' is expanded, but currently defmacro is not
         ;; itself a macro.  So both `defmacro' and `declare' need to be
         ;; handled directly in bytecomp.el.
         ;; FIXME: Maybe a simpler solution is to (defalias 'declare 'quote).
         (while (or (stringp (nth n form))
                    (eq (car-safe (nth n form)) 'declare))
           (setq n (1+ n)))
         (macroexpand-all-forms form n)))
      (`(defun . ,_) (macroexpand-all-forms form 3))
      (`(,(or `defvar `defconst) . ,_) (macroexpand-all-forms form 2))
      (`(function ,(and f `(lambda . ,_)))
       (maybe-cons 'function
                   (maybe-cons (macroexpand-all-forms f 2)
                               nil
                               (cdr form))
                   form))
      (`(,(or `function `quote) . ,_) form)
      (`(,(and fun (or `let `let*)) . ,(or `(,bindings . ,body) dontcare))
       (maybe-cons fun
                   (maybe-cons (macroexpand-all-clauses bindings 1)
                               (macroexpand-all-forms body)
                               (cdr form))
                   form))
      (`(,(and fun `(lambda . ,_)) . ,args)
       ;; Embedded lambda in function position.
       (maybe-cons (macroexpand-all-forms fun 2)
                   (macroexpand-all-forms args)
                   form))
      ;; The following few cases are for normal function calls that
      ;; are known to funcall one of their arguments.  The byte
      ;; compiler has traditionally handled these functions specially
      ;; by treating a lambda expression quoted by `quote' as if it
      ;; were quoted by `function'.  We make the same transformation
      ;; here, so that any code that cares about the difference will
      ;; see the same transformation.
      ;; First arg is a function:
      (`(,(and fun (or `funcall `apply `mapcar `mapatoms `mapconcat `mapc))
         ',(and f `(lambda . ,_)) . ,args)
       (byte-compile-log-warning
        (format "%s quoted with ' rather than with #'"
                (list 'lambda (nth 1 f) '...))
        t)
       ;; We don't use `maybe-cons' since there's clearly a change.
       (cons fun
             (cons (macroexpand-all-1 (list 'function f))
                   (macroexpand-all-forms args))))
      ;; Second arg is a function:
      (`(,(and fun (or `sort)) ,arg1 ',(and f `(lambda . ,_)) . ,args)
       (byte-compile-log-warning
        (format "%s quoted with ' rather than with #'"
                (list 'lambda (nth 1 f) '...))
        t)
       ;; We don't use `maybe-cons' since there's clearly a change.
       (cons fun
             (cons (macroexpand-all-1 arg1)
                   (cons (macroexpand-all-1
                          (list 'function f))
                         (macroexpand-all-forms args)))))
      ;; Macro expand compiler macros.  This cannot be delayed to
      ;; byte-optimize-form because the output of the compiler-macro can
      ;; use macros.
      ;; FIXME: Don't depend on CL.
      (`(,(pred (lambda (fun)
                  (and (symbolp fun)
                       (eq (get fun 'byte-compile)
                           'cl-byte-compile-compiler-macro)
                       (functionp 'compiler-macroexpand))))
         . ,_)
       (let ((newform (with-no-warnings (compiler-macroexpand form))))
         (if (eq form newform)
             (macroexpand-all-forms form 1)
           (macroexpand-all-1 newform))))
      (`(,_ . ,_)
       ;; For every other list, we just expand each argument (for
       ;; setq/setq-default this works alright because the variable names
       ;; are symbols).
       (macroexpand-all-forms form 1))
      (t form))))

;;;###autoload
(defun macroexpand-all (form &optional environment)
  "Return result of expanding macros at all levels in FORM.
If no macros are expanded, FORM is returned unchanged.
The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((macroexpand-all-environment environment))
    (macroexpand-all-1 form)))

(provide 'macroexp)

;;; macroexp.el ends here
