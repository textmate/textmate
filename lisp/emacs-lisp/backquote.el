;;; backquote.el --- implement the ` Lisp construct

;; Copyright (C) 1990, 1992, 1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Rick Sladkey <jrs@world.std.com>
;; Maintainer: FSF
;; Keywords: extensions, internal
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

;; When the Lisp reader sees `(...), it generates (\` (...)).
;; When it sees ,... inside such a backquote form, it generates (\, ...).
;; For ,@... it generates (\,@ ...).

;; This backquote will generate calls to the backquote-list* form.
;; Both a function version and a macro version are included.
;; The macro version is used by default because it is faster
;; and needs no run-time support.  It should really be a subr.

;;; Code:

(provide 'backquote)

;; function and macro versions of backquote-list*

(defun backquote-list*-function (first &rest list)
  "Like `list' but the last argument is the tail of the new list.

For example (backquote-list* 'a 'b 'c) => (a b . c)"
  ;; The recursive solution is much nicer:
  ;; (if list (cons first (apply 'backquote-list*-function list)) first))
  ;; but Emacs is not very good at efficiently processing recursion.
  (if list
      (let* ((rest list) (newlist (cons first nil)) (last newlist))
	(while (cdr rest)
	  (setcdr last (cons (car rest) nil))
	  (setq last (cdr last)
		rest (cdr rest)))
	(setcdr last (car rest))
	newlist)
    first))

(defmacro backquote-list*-macro (first &rest list)
  "Like `list' but the last argument is the tail of the new list.

For example (backquote-list* 'a 'b 'c) => (a b . c)"
  ;; The recursive solution is much nicer:
  ;; (if list (list 'cons first (cons 'backquote-list*-macro list)) first))
  ;; but Emacs is not very good at efficiently processing such things.
  (setq list (nreverse (cons first list))
	first (car list)
	list (cdr list))
  (if list
      (let* ((second (car list))
	     (rest (cdr list))
	     (newlist (list 'cons second first)))
	(while rest
	  (setq newlist (list 'cons (car rest) newlist)
		rest (cdr rest)))
	newlist)
    first))

(defalias 'backquote-list* (symbol-function 'backquote-list*-macro))

;; A few advertised variables that control which symbols are used
;; to represent the backquote, unquote, and splice operations.
(defconst backquote-backquote-symbol '\`
  "Symbol used to represent a backquote or nested backquote.")

(defconst backquote-unquote-symbol '\,
  "Symbol used to represent an unquote inside a backquote.")

(defconst backquote-splice-symbol '\,@
  "Symbol used to represent a splice inside a backquote.")

(defmacro backquote (structure)
  "Argument STRUCTURE describes a template to build.

The whole structure acts as if it were quoted except for certain
places where expressions are evaluated and inserted or spliced in.

For example:

b              => (ba bb bc)		; assume b has this value
`(a b c)       => (a b c)		; backquote acts like quote
`(a ,b c)      => (a (ba bb bc) c)	; insert the value of b
`(a ,@b c)     => (a ba bb bc c)	; splice in the value of b

Vectors work just like lists.  Nested backquotes are permitted."
  (cdr (backquote-process structure)))

;; GNU Emacs has no reader macros

(defalias '\` (symbol-function 'backquote))

;; backquote-process returns a dotted-pair of a tag (0, 1, or 2) and
;; the backquote-processed structure.  0 => the structure is
;; constant, 1 => to be unquoted, 2 => to be spliced in.
;; The top-level backquote macro just discards the tag.

(defun backquote-delay-process (s level)
  "Process a (un|back|splice)quote inside a backquote.
This simply recurses through the body."
  (let ((exp (backquote-listify (list (cons 0 (list 'quote (car s))))
                                (backquote-process (cdr s) level))))
    (if (eq (car-safe exp) 'quote)
        (cons 0 (list 'quote s))
      (cons 1 exp))))

(defun backquote-process (s &optional level)
  "Process the body of a backquote.
S is the body.  Returns a cons cell whose cdr is piece of code which
is the macro-expansion of S, and whose car is a small integer whose value
can either indicate that the code is constant (0), or not (1), or returns
a list which should be spliced into its environment (2).
LEVEL is only used internally and indicates the nesting level:
0 (the default) is for the toplevel nested inside a single backquote."
  (unless level (setq level 0))
  (cond
   ((vectorp s)
    (let ((n (backquote-process (append s ()) level)))
      (if (= (car n) 0)
	  (cons 0 s)
	(cons 1 (cond
		 ((not (listp (cdr n)))
		  (list 'vconcat (cdr n)))
		 ((eq (nth 1 n) 'list)
		  (cons 'vector (nthcdr 2 n)))
		 ((eq (nth 1 n) 'append)
		  (cons 'vconcat (nthcdr 2 n)))
		 (t
		  (list 'apply '(function vector) (cdr n))))))))
   ((atom s)
    (cons 0 (if (or (null s) (eq s t) (not (symbolp s)))
		s
	      (list 'quote s))))
   ((eq (car s) backquote-unquote-symbol)
    (if (<= level 0)
        (cons 1 (nth 1 s))
      (backquote-delay-process s (1- level))))
   ((eq (car s) backquote-splice-symbol)
    (if (<= level 0)
        (cons 2 (nth 1 s))
      (backquote-delay-process s (1- level))))
   ((eq (car s) backquote-backquote-symbol)
      (backquote-delay-process s (1+ level)))
   (t
    (let ((rest s)
	  item firstlist list lists expression)
      ;; Scan this list-level, setting LISTS to a list of forms,
      ;; each of which produces a list of elements
      ;; that should go in this level.
      ;; The order of LISTS is backwards.
      ;; If there are non-splicing elements (constant or variable)
      ;; at the beginning, put them in FIRSTLIST,
      ;; as a list of tagged values (TAG . FORM).
      ;; If there are any at the end, they go in LIST, likewise.
      (while (and (consp rest)
                  ;; Stop if the cdr is an expression inside a backquote or
                  ;; unquote since this needs to go recursively through
                  ;; backquote-process.
                  (not (or (eq (car rest) backquote-unquote-symbol)
                           (eq (car rest) backquote-backquote-symbol))))
	(setq item (backquote-process (car rest) level))
	(cond
	 ((= (car item) 2)
	  ;; Put the nonspliced items before the first spliced item
	  ;; into FIRSTLIST.
	  (if (null lists)
	      (setq firstlist list
		    list nil))
	  ;; Otherwise, put any preceding nonspliced items into LISTS.
	  (if list
	      (push (backquote-listify list '(0 . nil)) lists))
	  (push (cdr item) lists)
	  (setq list nil))
	 (t
	  (setq list (cons item list))))
	(setq rest (cdr rest)))
      ;; Handle nonsplicing final elements, and the tail of the list
      ;; (which remains in REST).
      (if (or rest list)
	  (push (backquote-listify list (backquote-process rest level))
                lists))
      ;; Turn LISTS into a form that produces the combined list.
      (setq expression
	    (if (or (cdr lists)
		    (eq (car-safe (car lists)) backquote-splice-symbol))
		(cons 'append (nreverse lists))
	      (car lists)))
      ;; Tack on any initial elements.
      (if firstlist
	  (setq expression (backquote-listify firstlist (cons 1 expression))))
      (if (eq (car-safe expression) 'quote)
	  (cons 0 (list 'quote s))
	(cons 1 expression))))))

;; backquote-listify takes (tag . structure) pairs from backquote-process
;; and decides between append, list, backquote-list*, and cons depending
;; on which tags are in the list.

(defun backquote-listify (list old-tail)
  (let ((heads nil) (tail (cdr old-tail)) (list-tail list) (item nil))
    (if (= (car old-tail) 0)
	(setq tail (eval tail)
	      old-tail nil))
    (while (consp list-tail)
      (setq item (car list-tail))
      (setq list-tail (cdr list-tail))
      (if (or heads old-tail (/= (car item) 0))
	  (setq heads (cons (cdr item) heads))
	(setq tail (cons (eval (cdr item)) tail))))
    (cond
     (tail
      (if (null old-tail)
	  (setq tail (list 'quote tail)))
      (if heads
	  (let ((use-list* (or (cdr heads)
			       (and (consp (car heads))
				    (eq (car (car heads))
					backquote-splice-symbol)))))
	    (cons (if use-list* 'backquote-list* 'cons)
		  (append heads (list tail))))
	tail))
     (t (cons 'list heads)))))

;;; backquote.el ends here
