;;; cl-compat.el --- Common Lisp extensions for GNU Emacs Lisp (compatibility)

;; Copyright (C) 1993, 2001-2012  Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
;; Keywords: extensions
;; Obsolete-since: 23.3

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

;; This file has been obsolete since Emacs 23.3.

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; This package works with Emacs 18, Emacs 19, and Lucid Emacs 19.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains emulations of internal routines of the older
;; CL package which users may have called directly from their code.
;; Use (require 'cl-compat) to get these routines.

;; See cl.el for Change Log.


;;; Code:

;; This used to be:
;; (or (featurep 'cl) (require 'cl))
;; which just has the effect of fooling the byte-compiler into not
;; loading cl when compiling.  However, that leads to some bogus
;; compiler warnings.  Loading cl when compiling cannot do any harm,
;; because for a long time bootstrap-emacs contained 'cl, due to being
;; dumped from uncompiled files that eval-when-compile'd cl.  So every
;; file was compiled with 'cl loaded.
(require 'cl)


;;; Keyword routines not supported by new package.

(defmacro defkeyword (x &optional doc)
  (list* 'defconst x (list 'quote x) (and doc (list doc))))

(defun keyword-of (sym)
  (or (keywordp sym) (keywordp (intern (format ":%s" sym)))))


;;; Multiple values.  Note that the new package uses a different
;;; convention for multiple values.  The following definitions
;;; emulate the old convention; all function names have been changed
;;; by capitalizing the first letter: Values, Multiple-value-*,
;;; to avoid conflict with the new-style definitions in cl-macs.

(defvar *mvalues-values* nil)

(defun Values (&rest val-forms)
  (setq *mvalues-values* val-forms)
  (car val-forms))

(defun Values-list (val-forms)
  (apply 'values val-forms))

(defmacro Multiple-value-list (form)
  (list 'let* (list '(*mvalues-values* nil) (list '*mvalues-temp* form))
	'(or (and (eq *mvalues-temp* (car *mvalues-values*)) *mvalues-values*)
	     (list *mvalues-temp*))))

(defmacro Multiple-value-call (function &rest args)
  (declare (indent 1))
  (list 'apply function
	(cons 'append
	      (mapcar (function (lambda (x) (list 'Multiple-value-list x)))
		      args))))

(defmacro Multiple-value-bind (vars form &rest body)
  (declare (indent 2))
  (list* 'multiple-value-bind vars (list 'Multiple-value-list form) body))

(defmacro Multiple-value-setq (vars form)
  (declare (indent 2))
  (list 'multiple-value-setq vars (list 'Multiple-value-list form)))

(defmacro Multiple-value-prog1 (form &rest body)
  (declare (indent 1))
  (list 'prog1 form (list* 'let '((*mvalues-values* nil)) body)))


;;; Routines for parsing keyword arguments.

(defun build-klist (arglist keys &optional allow-others)
  (let ((res (Multiple-value-call 'mapcar* 'cons (unzip-lists arglist))))
    (or allow-others
	(let ((bad (set-difference (mapcar 'car res) keys)))
	  (if bad (error "Bad keywords: %s not in %s" bad keys))))
    res))

(defun extract-from-klist (klist key &optional def)
  (let ((res (assq key klist))) (if res (cdr res) def)))

(defun keyword-argument-supplied-p (klist key)
  (assq key klist))

(defun elt-satisfies-test-p (item elt klist)
  (let ((test-not (cdr (assq ':test-not klist)))
	(test (cdr (assq ':test klist)))
	(key (cdr (assq ':key klist))))
    (if key (setq elt (funcall key elt)))
    (if test-not (not (funcall test-not item elt))
      (funcall (or test 'eql) item elt))))


;;; Rounding functions with old-style multiple value returns.

(defun cl-floor (a &optional b) (Values-list (floor* a b)))
(defun cl-ceiling (a &optional b) (Values-list (ceiling* a b)))
(defun cl-round (a &optional b) (Values-list (round* a b)))
(defun cl-truncate (a &optional b) (Values-list (truncate* a b)))

(defun safe-idiv (a b)
  (let* ((q (/ (abs a) (abs b)))
         (s (* (signum a) (signum b))))
    (Values q (- a (* s q b)) s)))


;; Internal routines.

(defun pair-with-newsyms (oldforms)
  (let ((newsyms (mapcar (lambda (x) (make-symbol "--cl-var--")) oldforms)))
    (Values (mapcar* 'list newsyms oldforms) newsyms)))

(defun zip-lists (evens odds)
  (mapcan 'list evens odds))

(defun unzip-lists (list)
  (let ((e nil) (o nil))
    (while list
      (setq e (cons (car list) e) o (cons (cadr list) o) list (cddr list)))
    (Values (nreverse e) (nreverse o))))

(defun reassemble-argslists (list)
  (let ((n (apply 'min (mapcar 'length list))) (res nil))
    (while (>= (setq n (1- n)) 0)
      (setq res (cons (mapcar (function (lambda (x) (elt x n))) list) res)))
    res))

(defun duplicate-symbols-p (list)
  (let ((res nil))
    (while list
      (if (memq (car list) (cdr list)) (setq res (cons (car list) res)))
      (setq list (cdr list)))
    res))


;;; Setf internals.

(defun setnth (n list x)
  (setcar (nthcdr n list) x))

(defun setnthcdr (n list x)
  (setcdr (nthcdr (1- n) list) x))

(defun setelt (seq n x)
  (if (consp seq) (setcar (nthcdr n seq) x) (aset seq n x)))


;;; Functions omitted: case-clausify, check-do-stepforms, check-do-endforms,
;;; extract-do-inits, extract-do[*]-steps, select-stepping-forms,
;;; elt-satisfies-if[-not]-p, with-keyword-args, mv-bind-clausify,
;;; all names with embedded `$'.


(provide 'cl-compat)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; cl-compat.el ends here
