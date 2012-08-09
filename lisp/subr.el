;;; subr.el --- basic lisp subroutines for Emacs  -*- coding: utf-8 -*-

;; Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2012
;;   Free Software Foundation, Inc.

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

;;; Code:

(defvar custom-declare-variable-list nil
  "Record `defcustom' calls made before `custom.el' is loaded to handle them.
Each element of this list holds the arguments to one call to `defcustom'.")

;; Use this, rather than defcustom, in subr.el and other files loaded
;; before custom.el.
(defun custom-declare-variable-early (&rest arguments)
  (setq custom-declare-variable-list
	(cons arguments custom-declare-variable-list)))

(defmacro declare-function (fn file &optional arglist fileonly)
  "Tell the byte-compiler that function FN is defined, in FILE.
Optional ARGLIST is the argument list used by the function.  The
FILE argument is not used by the byte-compiler, but by the
`check-declare' package, which checks that FILE contains a
definition for FN.  ARGLIST is used by both the byte-compiler and
`check-declare' to check for consistency.

FILE can be either a Lisp file (in which case the \".el\"
extension is optional), or a C file.  C files are expanded
relative to the Emacs \"src/\" directory.  Lisp files are
searched for using `locate-library', and if that fails they are
expanded relative to the location of the file containing the
declaration.  A FILE with an \"ext:\" prefix is an external file.
`check-declare' will check such files if they are found, and skip
them without error if they are not.

FILEONLY non-nil means that `check-declare' will only check that
FILE exists, not that it defines FN.  This is intended for
function-definitions that `check-declare' does not recognize, e.g.
`defstruct'.

To specify a value for FILEONLY without passing an argument list,
set ARGLIST to t.  This is necessary because nil means an
empty argument list, rather than an unspecified one.

Note that for the purposes of `check-declare', this statement
must be the first non-whitespace on a line.

For more information, see Info node `(elisp)Declaring Functions'."
  ;; Does nothing - byte-compile-declare-function does the work.
  nil)


;;;; Basic Lisp macros.

(defalias 'not 'null)

(defmacro noreturn (form)
  "Evaluate FORM, expecting it not to return.
If FORM does return, signal an error."
  `(prog1 ,form
     (error "Form marked with `noreturn' did return")))

(defmacro 1value (form)
  "Evaluate FORM, expecting a constant return value.
This is the global do-nothing version.  There is also `testcover-1value'
that complains if FORM ever does return differing values."
  form)

(defmacro def-edebug-spec (symbol spec)
  "Set the `edebug-form-spec' property of SYMBOL according to SPEC.
Both SYMBOL and SPEC are unevaluated.  The SPEC can be:
0 (instrument no arguments); t (instrument all arguments);
a symbol (naming a function with an Edebug specification); or a list.
The elements of the list describe the argument types; see
Info node `(elisp)Specification List' for details."
  `(put (quote ,symbol) 'edebug-form-spec (quote ,spec)))

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
`funcall' or `mapcar', etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of Lisp expressions.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))

(defun apply-partially (fun &rest args)
  "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
  `(closure (t) (&rest args)
            (apply ',fun ,@(mapcar (lambda (arg) `',arg) args) args)))

(if (null (featurep 'cl))
    (progn
  ;; If we reload subr.el after having loaded CL, be careful not to
  ;; overwrite CL's extended definition of `dolist', `dotimes',
  ;; `declare', `push' and `pop'.
(defmacro push (newelt listname)
  "Add NEWELT to the list stored in the symbol LISTNAME.
This is equivalent to (setq LISTNAME (cons NEWELT LISTNAME)).
LISTNAME must be a symbol."
  (declare (debug (form sexp)))
  (list 'setq listname
        (list 'cons newelt listname)))

(defmacro pop (listname)
  "Return the first element of LISTNAME's value, and remove it from the list.
LISTNAME must be a symbol whose value is a list.
If the value is nil, `pop' returns nil but does not actually
change the list."
  (declare (debug (sexp)))
  (list 'car
        (list 'prog1 listname
              (list 'setq listname (list 'cdr listname)))))
))

(defmacro when (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil.
When COND yields non-nil, eval BODY forms sequentially and return
value of last one, or nil if there are none.

\(fn COND BODY...)"
  (declare (indent 1) (debug t))
  (list 'if cond (cons 'progn body)))

(defmacro unless (cond &rest body)
  "If COND yields nil, do BODY, else return nil.
When COND yields nil, eval BODY forms sequentially and return
value of last one, or nil if there are none.

\(fn COND BODY...)"
  (declare (indent 1) (debug t))
  (cons 'if (cons cond (cons nil body))))

(if (null (featurep 'cl))
    (progn
  ;; If we reload subr.el after having loaded CL, be careful not to
  ;; overwrite CL's extended definition of `dolist', `dotimes',
  ;; `declare', `push' and `pop'.

(defmacro dolist (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  ;; It would be cleaner to create an uninterned symbol,
  ;; but that uses a lot more space when many functions in many files
  ;; use dolist.
  ;; FIXME: This cost disappears in byte-compiled lexical-binding files.
  (let ((temp '--dolist-tail--))
    ;; This is not a reliable test, but it does not matter because both
    ;; semantics are acceptable, tho one is slightly faster with dynamic
    ;; scoping and the other is slightly faster (and has cleaner semantics)
    ;; with lexical scoping.
    (if lexical-binding
        `(let ((,temp ,(nth 1 spec)))
           (while ,temp
             (let ((,(car spec) (car ,temp)))
               ,@body
               (setq ,temp (cdr ,temp))))
           ,@(if (cdr (cdr spec))
                 ;; FIXME: This let often leads to "unused var" warnings.
                 `((let ((,(car spec) nil)) ,@(cdr (cdr spec))))))
      `(let ((,temp ,(nth 1 spec))
             ,(car spec))
         (while ,temp
           (setq ,(car spec) (car ,temp))
           ,@body
           (setq ,temp (cdr ,temp)))
         ,@(if (cdr (cdr spec))
               `((setq ,(car spec) nil) ,@(cdr (cdr spec))))))))

(defmacro dotimes (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY with VAR bound to successive integers running from 0,
inclusive, to COUNT, exclusive.  Then evaluate RESULT to get
the return value (nil if RESULT is omitted).

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent 1) (debug dolist))
  ;; It would be cleaner to create an uninterned symbol,
  ;; but that uses a lot more space when many functions in many files
  ;; use dotimes.
  ;; FIXME: This cost disappears in byte-compiled lexical-binding files.
  (let ((temp '--dotimes-limit--)
	(start 0)
	(end (nth 1 spec)))
    ;; This is not a reliable test, but it does not matter because both
    ;; semantics are acceptable, tho one is slightly faster with dynamic
    ;; scoping and the other has cleaner semantics.
    (if lexical-binding
        (let ((counter '--dotimes-counter--))
          `(let ((,temp ,end)
                 (,counter ,start))
             (while (< ,counter ,temp)
               (let ((,(car spec) ,counter))
                 ,@body)
               (setq ,counter (1+ ,counter)))
             ,@(if (cddr spec)
                   ;; FIXME: This let often leads to "unused var" warnings.
                   `((let ((,(car spec) ,counter)) ,@(cddr spec))))))
      `(let ((,temp ,end)
             (,(car spec) ,start))
         (while (< ,(car spec) ,temp)
           ,@body
           (setq ,(car spec) (1+ ,(car spec))))
         ,@(cdr (cdr spec))))))

(defmacro declare (&rest _specs)
  "Do not evaluate any arguments and return nil.
Treated as a declaration when used at the right place in a
`defmacro' form.  \(See Info anchor `(elisp)Definition of declare'.)"
  nil)
))

(defmacro ignore-errors (&rest body)
  "Execute BODY; if an error occurs, return nil.
Otherwise, return result of last form in BODY."
  (declare (debug t) (indent 0))
  `(condition-case nil (progn ,@body) (error nil)))

;;;; Basic Lisp functions.

(defun ignore (&rest _ignore)
  "Do nothing and return nil.
This function accepts any number of arguments, but ignores them."
  (interactive)
  nil)

;; Signal a compile-error if the first arg is missing.
(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'.
In Emacs, the convention is that error messages start with a capital
letter but *do not* end with a period.  Please follow this convention
for the sake of consistency."
  (while t
    (signal 'error (list (apply 'format args)))))
(set-advertised-calling-convention 'error '(string &rest args) "23.1")

;; We put this here instead of in frame.el so that it's defined even on
;; systems where frame.el isn't loaded.
(defun frame-configuration-p (object)
  "Return non-nil if OBJECT seems to be a frame configuration.
Any list whose car is `frame-configuration' is assumed to be a frame
configuration."
  (and (consp object)
       (eq (car object) 'frame-configuration)))

;;;; List functions.

(defsubst caar (x)
  "Return the car of the car of X."
  (car (car x)))

(defsubst cadr (x)
  "Return the car of the cdr of X."
  (car (cdr x)))

(defsubst cdar (x)
  "Return the cdr of the car of X."
  (cdr (car x)))

(defsubst cddr (x)
  "Return the cdr of the cdr of X."
  (cdr (cdr x)))

(defun last (list &optional n)
  "Return the last link of LIST.  Its car is the last element.
If LIST is nil, return nil.
If N is non-nil, return the Nth-to-last link of LIST.
If N is bigger than the length of LIST, return LIST."
  (if n
      (and (>= n 0)
           (let ((m (safe-length list)))
             (if (< n m) (nthcdr (- m n) list) list)))
    (and list
         (nthcdr (1- (safe-length list)) list))))

(defun butlast (list &optional n)
  "Return a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) list
    (nbutlast (copy-sequence list) n)))

(defun nbutlast (list &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length list)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) list) nil))
	   list))))

(defun delete-dups (list)
  "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
  (let ((tail list))
    (while tail
      (setcdr tail (delete (car tail) (cdr tail)))
      (setq tail (cdr tail))))
  list)

(defun number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return \(FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list \(0.4),
whereas \(number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as \(+ FROM \(* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative)."
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (> inc 0)
          (while (<= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc))))
        (while (>= next to)
          (setq seq (cons next seq)
                n (1+ n)
                next (+ from (* n inc)))))
      (nreverse seq))))

(defun copy-tree (tree &optional vecp)
  "Make a copy of TREE.
If TREE is a cons cell, this recursively copies both its car and its cdr.
Contrast to `copy-sequence', which copies only along the cdrs.  With second
argument VECP, this copies vectors as well as conses."
  (if (consp tree)
      (let (result)
	(while (consp tree)
	  (let ((newcar (car tree)))
	    (if (or (consp (car tree)) (and vecp (vectorp (car tree))))
		(setq newcar (copy-tree (car tree) vecp)))
	    (push newcar result))
	  (setq tree (cdr tree)))
	(nconc (nreverse result) tree))
    (if (and vecp (vectorp tree))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (copy-tree (aref tree i) vecp)))
	  tree)
      tree)))

;;;; Various list-search functions.

(defun assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element
 (or the element's car, if it is a cons) is compared with KEY by
 calling TEST, with two arguments: (i) the element or its car,
 and (ii) KEY.
If that is non-nil, the element matches; then `assoc-default'
 returns the element's cdr, if it is a cons, or DEFAULT if the
 element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) (cdr elt) default))))
      (setq tail (cdr tail)))
    value))

(make-obsolete 'assoc-ignore-case 'assoc-string "22.1")
(defun assoc-ignore-case (key alist)
  "Like `assoc', but ignores differences in case and text representation.
KEY must be a string.  Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
  (assoc-string key alist t))

(make-obsolete 'assoc-ignore-representation 'assoc-string "22.1")
(defun assoc-ignore-representation (key alist)
  "Like `assoc', but ignores differences in text representation.
KEY must be a string.
Unibyte strings are converted to multibyte for comparison."
  (assoc-string key alist nil))

(defun member-ignore-case (elt list)
  "Like `member', but ignore differences in case and text representation.
ELT must be a string.  Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison.
Non-strings in LIST are ignored."
  (while (and list
	      (not (and (stringp (car list))
			(eq t (compare-strings elt 0 nil (car list) 0 nil t)))))
    (setq list (cdr list)))
  list)

(defun assq-delete-all (key alist)
  "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (eq (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (eq (car (car tail-cdr)) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun rassq-delete-all (value alist)
  "Delete from ALIST all elements whose cdr is `eq' to VALUE.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (eq (cdr (car alist)) value))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (eq (cdr (car tail-cdr)) value))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun remove (elt seq)
  "Return a copy of SEQ with all occurrences of ELT removed.
SEQ must be a list, vector, or string.  The comparison is done with `equal'."
  (if (nlistp seq)
      ;; If SEQ isn't a list, there's no need to copy SEQ because
      ;; `delete' will return a new object.
      (delete elt seq)
    (delete elt (copy-sequence seq))))

(defun remq (elt list)
  "Return LIST with all occurrences of ELT removed.
The comparison is done with `eq'.  Contrary to `delq', this does not use
side-effects, and the argument LIST is not modified."
  (while (and (eq elt (car list)) (setq list (cdr list))))
  (if (memq elt list)
      (delq elt (copy-sequence list))
    list))

;;;; Keymap support.

(defmacro kbd (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `edmacro-mode')."
  (read-kbd-macro keys))

(defun undefined ()
  "Beep to tell the user this binding is undefined."
  (interactive)
  (ding))

;; Prevent the \{...} documentation construct
;; from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars."
  (define-key map [remap self-insert-command] 'undefined)
  (or nodigits
      (let (loop)
	(define-key map "-" 'negative-argument)
	;; Make plain numbers do numeric args.
	(setq loop ?0)
	(while (<= loop ?9)
	  (define-key map (char-to-string loop) 'digit-argument)
	  (setq loop (1+ loop))))))

(defun make-composed-keymap (maps &optional parent)
  "Construct a new keymap composed of MAPS and inheriting from PARENT.
When looking up a key in the returned map, the key is looked in each
keymap of MAPS in turn until a binding is found.
If no binding is found in MAPS, the lookup continues in PARENT, if non-nil.
As always with keymap inheritance, a nil binding in MAPS overrides
any corresponding binding in PARENT, but it does not override corresponding
bindings in other keymaps of MAPS.
MAPS can be a list of keymaps or a single keymap.
PARENT if non-nil should be a keymap."
  `(keymap
    ,@(if (keymapp maps) (list maps) maps)
    ,@parent))

(defun define-key-after (keymap key definition &optional after)
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `define-key' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t or omitted, the new binding goes at the end of the keymap.
AFTER should be a single event type--a symbol or a character, not a sequence.

Bindings are always added before any inherited map.

The order of bindings in a keymap only matters when it is used as
a menu, so this function is not useful for non-menu keymaps."
  (unless after (setq after t))
  (or (keymapp keymap)
      (signal 'wrong-type-argument (list 'keymapp keymap)))
  (setq key
	(if (<= (length key) 1) (aref key 0)
	  (setq keymap (lookup-key keymap
				   (apply 'vector
					  (butlast (mapcar 'identity key)))))
	  (aref key (1- (length key)))))
  (let ((tail keymap) done inserted)
    (while (and (not done) tail)
      ;; Delete any earlier bindings for the same key.
      (if (eq (car-safe (car (cdr tail))) key)
	  (setcdr tail (cdr (cdr tail))))
      ;; If we hit an included map, go down that one.
      (if (keymapp (car tail)) (setq tail (car tail)))
      ;; When we reach AFTER's binding, insert the new binding after.
      ;; If we reach an inherited keymap, insert just before that.
      ;; If we reach the end of this keymap, insert at the end.
      (if (or (and (eq (car-safe (car tail)) after)
		   (not (eq after t)))
	      (eq (car (cdr tail)) 'keymap)
	      (null (cdr tail)))
	  (progn
	    ;; Stop the scan only if we find a parent keymap.
	    ;; Keep going past the inserted element
	    ;; so we can delete any duplications that come later.
	    (if (eq (car (cdr tail)) 'keymap)
		(setq done t))
	    ;; Don't insert more than once.
	    (or inserted
		(setcdr tail (cons (cons key definition) (cdr tail))))
	    (setq inserted t)))
      (setq tail (cdr tail)))))

(defun map-keymap-sorted (function keymap)
  "Implement `map-keymap' with sorting.
Don't call this function; it is for internal use only."
  (let (list)
    (map-keymap (lambda (a b) (push (cons a b) list))
                keymap)
    (setq list (sort list
                     (lambda (a b)
                       (setq a (car a) b (car b))
                       (if (integerp a)
                           (if (integerp b) (< a b)
                             t)
                         (if (integerp b) t
                           ;; string< also accepts symbols.
                           (string< a b))))))
    (dolist (p list)
      (funcall function (car p) (cdr p)))))

(defun keymap--menu-item-binding (val)
  "Return the binding part of a menu-item."
  (cond
   ((not (consp val)) val)              ;Not a menu-item.
   ((eq 'menu-item (car val))
    (let* ((binding (nth 2 val))
           (plist (nthcdr 3 val))
           (filter (plist-get plist :filter)))
      (if filter (funcall filter binding)
        binding)))
   ((and (consp (cdr val)) (stringp (cadr val)))
    (cddr val))
   ((stringp (car val))
    (cdr val))
   (t val)))                            ;Not a menu-item either.

(defun keymap--menu-item-with-binding (item binding)
  "Build a menu-item like ITEM but with its binding changed to BINDING."
  (cond
   ((not (consp item)) binding)		;Not a menu-item.
   ((eq 'menu-item (car item))
    (setq item (copy-sequence item))
    (let ((tail (nthcdr 2 item)))
      (setcar tail binding)
      ;; Remove any potential filter.
      (if (plist-get (cdr tail) :filter)
          (setcdr tail (plist-put (cdr tail) :filter nil))))
    item)
   ((and (consp (cdr item)) (stringp (cadr item)))
    (cons (car item) (cons (cadr item) binding)))
   (t (cons (car item) binding))))

(defun keymap--merge-bindings (val1 val2)
  "Merge bindings VAL1 and VAL2."
  (let ((map1 (keymap--menu-item-binding val1))
        (map2 (keymap--menu-item-binding val2)))
    (if (not (and (keymapp map1) (keymapp map2)))
        ;; There's nothing to merge: val1 takes precedence.
        val1
      (let ((map (list 'keymap map1 map2))
            (item (if (keymapp val1) (if (keymapp val2) nil val2) val1)))
        (keymap--menu-item-with-binding item map)))))

(defun keymap-canonicalize (map)
  "Return a simpler equivalent keymap.
This resolves inheritance and redefinitions.  The returned keymap
should behave identically to a copy of KEYMAP w.r.t `lookup-key'
and use in active keymaps and menus.
Subkeymaps may be modified but are not canonicalized."
  ;; FIXME: Problem with the difference between a nil binding
  ;; that hides a binding in an inherited map and a nil binding that's ignored
  ;; to let some further binding visible.  Currently a nil binding hides all.
  ;; FIXME: we may want to carefully (re)order elements in case they're
  ;; menu-entries.
  (let ((bindings ())
        (ranges ())
	(prompt (keymap-prompt map)))
    (while (keymapp map)
      (setq map (map-keymap ;; -internal
                 (lambda (key item)
                   (if (consp key)
                       ;; Treat char-ranges specially.
                       (push (cons key item) ranges)
                     (push (cons key item) bindings)))
                 map)))
    ;; Create the new map.
    (setq map (funcall (if ranges 'make-keymap 'make-sparse-keymap) prompt))
    (dolist (binding ranges)
      ;; Treat char-ranges specially.  FIXME: need to merge as well.
      (define-key map (vector (car binding)) (cdr binding)))
    ;; Process the bindings starting from the end.
    (dolist (binding (prog1 bindings (setq bindings ())))
      (let* ((key (car binding))
             (item (cdr binding))
             (oldbind (assq key bindings)))
        (push (if (not oldbind)
                  ;; The normal case: no duplicate bindings.
                  binding
                ;; This is the second binding for this key.
                (setq bindings (delq oldbind bindings))
                (cons key (keymap--merge-bindings (cdr binding)
                                                  (cdr oldbind))))
              bindings)))
    (nconc map bindings)))

(put 'keyboard-translate-table 'char-table-extra-slots 0)

(defun keyboard-translate (from to)
  "Translate character FROM to TO at a low level.
This function creates a `keyboard-translate-table' if necessary
and then modifies one entry in it."
  (or (char-table-p keyboard-translate-table)
      (setq keyboard-translate-table
	    (make-char-table 'keyboard-translate-table nil)))
  (aset keyboard-translate-table from to))

;;;; Key binding commands.

(defun global-set-key (key command)
  "Give KEY a global binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function."
  (interactive "KSet key globally: \nCSet key %s to command: ")
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key (current-global-map) key command))

(defun local-set-key (key command)
  "Give KEY a local binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  (interactive "KSet key locally: \nCSet key %s locally to command: ")
  (let ((map (current-local-map)))
    (or map
	(use-local-map (setq map (make-sparse-keymap))))
    (or (vectorp key) (stringp key)
	(signal 'wrong-type-argument (list 'arrayp key)))
    (define-key map key command)))

(defun global-unset-key (key)
  "Remove global binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key globally: ")
  (global-set-key key nil))

(defun local-unset-key (key)
  "Remove local binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key locally: ")
  (if (current-local-map)
      (local-set-key key nil))
  nil)

;;;; substitute-key-definition and its subroutines.

(defvar key-substitution-in-progress nil
  "Used internally by `substitute-key-definition'.")

(defun substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys which are defined as OLDDEF in OLDMAP.

If you don't specify OLDMAP, you can usually get the same results
in a cleaner way with command remapping, like this:
  \(define-key KEYMAP [remap OLDDEF] NEWDEF)
\n(fn OLDDEF NEWDEF KEYMAP &optional OLDMAP)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (or prefix (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (prefix1 (vconcat prefix [nil]))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(defun substitute-key-definition-key (defn olddef newdef prefix keymap)
  (let (inner-def skipped menu-item)
    ;; Find the actual command name within the binding.
    (if (eq (car-safe defn) 'menu-item)
	(setq menu-item defn defn (nth 2 defn))
      ;; Skip past menu-prompt.
      (while (stringp (car-safe defn))
	(push (pop defn) skipped))
      ;; Skip past cached key-equivalence data for menu items.
      (if (consp (car-safe defn))
	  (setq defn (cdr defn))))
    (if (or (eq defn olddef)
	    ;; Compare with equal if definition is a key sequence.
	    ;; That is useful for operating on function-key-map.
	    (and (or (stringp defn) (vectorp defn))
		 (equal defn olddef)))
	(define-key keymap prefix
	  (if menu-item
	      (let ((copy (copy-sequence menu-item)))
		(setcar (nthcdr 2 copy) newdef)
		copy)
	    (nconc (nreverse skipped) newdef)))
      ;; Look past a symbol that names a keymap.
      (setq inner-def
	    (or (indirect-function defn t) defn))
      ;; For nested keymaps, we use `inner-def' rather than `defn' so as to
      ;; avoid autoloading a keymap.  This is mostly done to preserve the
      ;; original non-autoloading behavior of pre-map-keymap times.
      (if (and (keymapp inner-def)
	       ;; Avoid recursively scanning
	       ;; where KEYMAP does not have a submap.
	       (let ((elt (lookup-key keymap prefix)))
		 (or (null elt) (natnump elt) (keymapp elt)))
	       ;; Avoid recursively rescanning keymap being scanned.
	       (not (memq inner-def key-substitution-in-progress)))
	  ;; If this one isn't being scanned already, scan it now.
	  (substitute-key-definition olddef newdef keymap inner-def prefix)))))


;;;; The global keymap tree.

;; global-map, esc-map, and ctl-x-map have their values set up in
;; keymap.c; we just give them docstrings here.

(defvar global-map nil
  "Default global keymap mapping Emacs keyboard input into commands.
The value is a keymap which is usually (but not necessarily) Emacs's
global map.")

(defvar esc-map nil
  "Default keymap for ESC (meta) commands.
The normal global definition of the character ESC indirects to this keymap.")

(defvar ctl-x-map nil
  "Default keymap for C-x commands.
The normal global definition of the character C-x indirects to this keymap.")

(defvar ctl-x-4-map (make-sparse-keymap)
  "Keymap for subcommands of C-x 4.")
(defalias 'ctl-x-4-prefix ctl-x-4-map)
(define-key ctl-x-map "4" 'ctl-x-4-prefix)

(defvar ctl-x-5-map (make-sparse-keymap)
  "Keymap for frame commands.")
(defalias 'ctl-x-5-prefix ctl-x-5-map)
(define-key ctl-x-map "5" 'ctl-x-5-prefix)


;;;; Event manipulation functions.

(defconst listify-key-sequence-1 (logior 128 ?\M-\C-@))

(defun listify-key-sequence (key)
  "Convert a key sequence to a list of events."
  (if (vectorp key)
      (append key nil)
    (mapcar (function (lambda (c)
			(if (> c 127)
			    (logxor c listify-key-sequence-1)
			  c)))
	    key)))

(defsubst eventp (obj)
  "True if the argument is an event object."
  (or (and (integerp obj)
	   ;; Filter out integers too large to be events.
	   ;; M is the biggest modifier.
	   (zerop (logand obj (lognot (1- (lsh ?\M-\^@ 1)))))
	   (characterp (event-basic-type obj)))
      (and (symbolp obj)
	   (get obj 'event-symbol-elements))
      (and (consp obj)
	   (symbolp (car obj))
	   (get (car obj) 'event-symbol-elements))))

(defun event-modifiers (event)
  "Return a list of symbols representing the modifier keys in event EVENT.
The elements of the list may include `meta', `control',
`shift', `hyper', `super', `alt', `click', `double', `triple', `drag',
and `down'.
EVENT may be an event or an event type.  If EVENT is a symbol
that has never been used in an event that has been read as input
in the current Emacs session, then this function may fail to include
the `click' modifier."
  (let ((type event))
    (if (listp type)
	(setq type (car type)))
    (if (symbolp type)
        ;; Don't read event-symbol-elements directly since we're not
        ;; sure the symbol has already been parsed.
	(cdr (internal-event-symbol-parse-modifiers type))
      (let ((list nil)
	    (char (logand type (lognot (logior ?\M-\^@ ?\C-\^@ ?\S-\^@
					       ?\H-\^@ ?\s-\^@ ?\A-\^@)))))
	(if (not (zerop (logand type ?\M-\^@)))
	    (push 'meta list))
	(if (or (not (zerop (logand type ?\C-\^@)))
		(< char 32))
	    (push 'control list))
	(if (or (not (zerop (logand type ?\S-\^@)))
		(/= char (downcase char)))
	    (push 'shift list))
	(or (zerop (logand type ?\H-\^@))
	    (push 'hyper list))
	(or (zerop (logand type ?\s-\^@))
	    (push 'super list))
	(or (zerop (logand type ?\A-\^@))
	    (push 'alt list))
	list))))

(defun event-basic-type (event)
  "Return the basic type of the given event (all modifiers removed).
The value is a printing character (not upper case) or a symbol.
EVENT may be an event or an event type.  If EVENT is a symbol
that has never been used in an event that has been read as input
in the current Emacs session, then this function may return nil."
  (if (consp event)
      (setq event (car event)))
  (if (symbolp event)
      (car (get event 'event-symbol-elements))
    (let* ((base (logand event (1- ?\A-\^@)))
	   (uncontrolled (if (< base 32) (logior base 64) base)))
      ;; There are some numbers that are invalid characters and
      ;; cause `downcase' to get an error.
      (condition-case ()
	  (downcase uncontrolled)
	(error uncontrolled)))))

(defsubst mouse-movement-p (object)
  "Return non-nil if OBJECT is a mouse movement event."
  (eq (car-safe object) 'mouse-movement))

(defun mouse-event-p (object)
  "Return non-nil if OBJECT is a mouse click event."
  ;; is this really correct? maybe remove mouse-movement?
  (memq (event-basic-type object) '(mouse-1 mouse-2 mouse-3 mouse-movement)))

(defsubst event-start (event)
  "Return the starting position of EVENT.
EVENT should be a click, drag, or key press event.
If it is a key press event, the return value has the form
    (WINDOW POS (0 . 0) 0)
If it is a click or drag event, it has the form
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The `posn-' functions access elements of such lists.
For more information, see Info node `(elisp)Click Events'.

If EVENT is a mouse or key press or a mouse click, this is the
position of the event.  If EVENT is a drag, this is the starting
position of the drag."
  (if (consp event) (nth 1 event)
    (list (selected-window) (point) '(0 . 0) 0)))

(defsubst event-end (event)
  "Return the ending location of EVENT.
EVENT should be a click, drag, or key press event.
If EVENT is a key press event, the return value has the form
    (WINDOW POS (0 . 0) 0)
If EVENT is a click event, this function is the same as
`event-start'.  For click and drag events, the return value has
the form
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The `posn-' functions access elements of such lists.
For more information, see Info node `(elisp)Click Events'.

If EVENT is a mouse or key press or a mouse click, this is the
position of the event.  If EVENT is a drag, this is the starting
position of the drag."
  (if (consp event) (nth (if (consp (nth 2 event)) 2 1) event)
    (list (selected-window) (point) '(0 . 0) 0)))

(defsubst event-click-count (event)
  "Return the multi-click count of EVENT, a click or drag event.
The return value is a positive integer."
  (if (and (consp event) (integerp (nth 2 event))) (nth 2 event) 1))

;;;; Extracting fields of the positions in an event.

(defsubst posn-window (position)
  "Return the window in POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 0 position))

(defsubst posn-area (position)
  "Return the window area recorded in POSITION, or nil for the text area.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (let ((area (if (consp (nth 1 position))
		  (car (nth 1 position))
		(nth 1 position))))
    (and (symbolp area) area)))

(defsubst posn-point (position)
  "Return the buffer location in POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (or (nth 5 position)
      (if (consp (nth 1 position))
	  (car (nth 1 position))
	(nth 1 position))))

(defun posn-set-point (position)
  "Move point to POSITION.
Select the corresponding window as well."
  (if (not (windowp (posn-window position)))
      (error "Position not in text area of window"))
  (select-window (posn-window position))
  (if (numberp (posn-point position))
      (goto-char (posn-point position))))

(defsubst posn-x-y (position)
  "Return the x and y coordinates in POSITION.
The return value has the form (X . Y), where X and Y are given in
pixels.  POSITION should be a list of the form returned by
`event-start' and `event-end'."
  (nth 2 position))

(declare-function scroll-bar-scale "scroll-bar" (num-denom whole))

(defun posn-col-row (position)
  "Return the nominal column and row in POSITION, measured in characters.
The column and row values are approximations calculated from the x
and y coordinates in POSITION and the frame's default character width
and height.
For a scroll-bar event, the result column is 0, and the row
corresponds to the vertical position of the click in the scroll bar.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (let* ((pair   (posn-x-y position))
	 (window (posn-window position))
	 (area   (posn-area position)))
    (cond
     ((null window)
      '(0 . 0))
     ((eq area 'vertical-scroll-bar)
      (cons 0 (scroll-bar-scale pair (1- (window-height window)))))
     ((eq area 'horizontal-scroll-bar)
      (cons (scroll-bar-scale pair (window-width window)) 0))
     (t
      (let* ((frame (if (framep window) window (window-frame window)))
	     ;; FIXME: This should take line-spacing properties on
	     ;; newlines into account.
	     (spacing (when (display-graphic-p frame)
			(or (with-current-buffer (window-buffer window)
			      line-spacing)
			    (frame-parameter frame 'line-spacing)))))
	(cond ((floatp spacing)
	       (setq spacing (truncate (* spacing
					  (frame-char-height frame)))))
	      ((null spacing)
	       (setq spacing 0)))
	(cons (/ (car pair) (frame-char-width frame))
	      (- (/ (cdr pair) (+ (frame-char-height frame) spacing))
		 (if (null (with-current-buffer (window-buffer window)
			     header-line-format))
		     0 1))))))))

(defun posn-actual-col-row (position)
  "Return the actual column and row in POSITION, measured in characters.
These are the actual row number in the window and character number in that row.
Return nil if POSITION does not contain the actual position; in that case
`posn-col-row' can be used to get approximate values.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 6 position))

(defsubst posn-timestamp (position)
  "Return the timestamp of POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 3 position))

(defsubst posn-string (position)
  "Return the string object of POSITION.
Value is a cons (STRING . STRING-POS), or nil if not a string.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 4 position))

(defsubst posn-image (position)
  "Return the image object of POSITION.
Value is a list (image ...), or nil if not an image.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 7 position))

(defsubst posn-object (position)
  "Return the object (image or string) of POSITION.
Value is a list (image ...) for an image object, a cons cell
\(STRING . STRING-POS) for a string object, and nil for a buffer position.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (or (posn-image position) (posn-string position)))

(defsubst posn-object-x-y (position)
  "Return the x and y coordinates relative to the object of POSITION.
The return value has the form (DX . DY), where DX and DY are
given in pixels.  POSITION should be a list of the form returned
by `event-start' and `event-end'."
  (nth 8 position))

(defsubst posn-object-width-height (position)
  "Return the pixel width and height of the object of POSITION.
The return value has the form (WIDTH . HEIGHT).  POSITION should
be a list of the form returned by `event-start' and `event-end'."
  (nth 9 position))


;;;; Obsolescent names for functions.

(define-obsolete-function-alias 'window-dot 'window-point "22.1")
(define-obsolete-function-alias 'set-window-dot 'set-window-point "22.1")
(define-obsolete-function-alias 'read-input 'read-string "22.1")
(define-obsolete-function-alias 'show-buffer 'set-window-buffer "22.1")
(define-obsolete-function-alias 'eval-current-buffer 'eval-buffer "22.1")
(define-obsolete-function-alias 'string-to-int 'string-to-number "22.1")

(make-obsolete 'forward-point "use (+ (point) N) instead." "23.1")

(defun insert-string (&rest args)
  "Mocklisp-compatibility insert function.
Like the function `insert' except that any argument that is a number
is converted into a string by expressing it in decimal."
  (dolist (el args)
    (insert (if (integerp el) (number-to-string el) el))))
(make-obsolete 'insert-string 'insert "22.1")

(defun makehash (&optional test) (make-hash-table :test (or test 'eql)))
(make-obsolete 'makehash 'make-hash-table "22.1")

;; These are used by VM and some old programs
(defalias 'focus-frame 'ignore "")
(make-obsolete 'focus-frame "it does nothing." "22.1")
(defalias 'unfocus-frame 'ignore "")
(make-obsolete 'unfocus-frame "it does nothing." "22.1")
(make-obsolete 'make-variable-frame-local
	       "explicitly check for a frame-parameter instead." "22.2")
(make-obsolete 'interactive-p 'called-interactively-p "23.2")
(set-advertised-calling-convention 'called-interactively-p '(kind) "23.1")
(set-advertised-calling-convention
 'all-completions '(string collection &optional predicate) "23.1")
(set-advertised-calling-convention 'unintern '(name obarray) "23.3")

;;;; Obsolescence declarations for variables, and aliases.

;; Special "default-FOO" variables which contain the default value of
;; the "FOO" variable are nasty.  Their implementation is brittle, and
;; slows down several unrelated variable operations; furthermore, they
;; can lead to really odd behavior if you decide to make them
;; buffer-local.

;; Not used at all in Emacs, last time I checked:
(make-obsolete-variable 'default-mode-line-format 'mode-line-format "23.2")
(make-obsolete-variable 'default-header-line-format 'header-line-format "23.2")
(make-obsolete-variable 'default-line-spacing 'line-spacing "23.2")
(make-obsolete-variable 'default-abbrev-mode 'abbrev-mode "23.2")
(make-obsolete-variable 'default-ctl-arrow 'ctl-arrow "23.2")
(make-obsolete-variable 'default-truncate-lines 'truncate-lines "23.2")
(make-obsolete-variable 'default-left-margin 'left-margin "23.2")
(make-obsolete-variable 'default-tab-width 'tab-width "23.2")
(make-obsolete-variable 'default-case-fold-search 'case-fold-search "23.2")
(make-obsolete-variable 'default-left-margin-width 'left-margin-width "23.2")
(make-obsolete-variable 'default-right-margin-width 'right-margin-width "23.2")
(make-obsolete-variable 'default-left-fringe-width 'left-fringe-width "23.2")
(make-obsolete-variable 'default-right-fringe-width 'right-fringe-width "23.2")
(make-obsolete-variable 'default-fringes-outside-margins 'fringes-outside-margins "23.2")
(make-obsolete-variable 'default-scroll-bar-width 'scroll-bar-width "23.2")
(make-obsolete-variable 'default-vertical-scroll-bar 'vertical-scroll-bar "23.2")
(make-obsolete-variable 'default-indicate-empty-lines 'indicate-empty-lines "23.2")
(make-obsolete-variable 'default-indicate-buffer-boundaries 'indicate-buffer-boundaries "23.2")
(make-obsolete-variable 'default-fringe-indicator-alist 'fringe-indicator-alist "23.2")
(make-obsolete-variable 'default-fringe-cursor-alist 'fringe-cursor-alist "23.2")
(make-obsolete-variable 'default-scroll-up-aggressively 'scroll-up-aggressively "23.2")
(make-obsolete-variable 'default-scroll-down-aggressively 'scroll-down-aggressively "23.2")
(make-obsolete-variable 'default-fill-column 'fill-column "23.2")
(make-obsolete-variable 'default-cursor-type 'cursor-type "23.2")
(make-obsolete-variable 'default-buffer-file-type 'buffer-file-type "23.2")
(make-obsolete-variable 'default-cursor-in-non-selected-windows 'cursor-in-non-selected-windows "23.2")
(make-obsolete-variable 'default-buffer-file-coding-system 'buffer-file-coding-system "23.2")
(make-obsolete-variable 'default-major-mode 'major-mode "23.2")
(make-obsolete-variable 'default-enable-multibyte-characters
      "use enable-multibyte-characters or set-buffer-multibyte instead" "23.2")

(make-obsolete-variable 'define-key-rebound-commands nil "23.2")
(make-obsolete-variable 'redisplay-end-trigger-functions 'jit-lock-register "23.1")
(make-obsolete-variable 'deferred-action-list 'post-command-hook "24.1")
(make-obsolete-variable 'deferred-action-function 'post-command-hook "24.1")
(make-obsolete 'window-redisplay-end-trigger nil "23.1")
(make-obsolete 'set-window-redisplay-end-trigger nil "23.1")

(make-obsolete 'process-filter-multibyte-p nil "23.1")
(make-obsolete 'set-process-filter-multibyte nil "23.1")

(make-obsolete-variable
 'mode-line-inverse-video
 "use the appropriate faces instead."
 "21.1")
(make-obsolete-variable
 'unread-command-char
 "use `unread-command-events' instead.  That variable is a list of events
to reread, so it now uses nil to mean `no event', instead of -1."
 "before 19.15")

;; Lisp manual only updated in 22.1.
(define-obsolete-variable-alias 'executing-macro 'executing-kbd-macro
  "before 19.34")

(defvaralias 'x-lost-selection-hooks 'x-lost-selection-functions)
(make-obsolete-variable 'x-lost-selection-hooks
			'x-lost-selection-functions "22.1")
(defvaralias 'x-sent-selection-hooks 'x-sent-selection-functions)
(make-obsolete-variable 'x-sent-selection-hooks
			'x-sent-selection-functions "22.1")

;; This was introduced in 21.4 for pre-unicode unification.  That
;; usage was rendered obsolete in 23.1 which uses Unicode internally.
;; Other uses are possible, so this variable is not _really_ obsolete,
;; but Stefan insists to mark it so.
(make-obsolete-variable 'translation-table-for-input nil "23.1")

(defvaralias 'messages-buffer-max-lines 'message-log-max)

;; These aliases exist in Emacs 19.34, and probably before, but were
;; only marked as obsolete in 23.1.
;; The lisp manual (since at least Emacs 21) describes them as
;; existing "for compatibility with Emacs version 18".
(define-obsolete-variable-alias 'last-input-char 'last-input-event
  "at least 19.34")
(define-obsolete-variable-alias 'last-command-char 'last-command-event
  "at least 19.34")


;;;; Alternate names for functions - these are not being phased out.

(defalias 'send-string 'process-send-string)
(defalias 'send-region 'process-send-region)
(defalias 'string= 'string-equal)
(defalias 'string< 'string-lessp)
(defalias 'move-marker 'set-marker)
(defalias 'rplaca 'setcar)
(defalias 'rplacd 'setcdr)
(defalias 'beep 'ding) ;preserve lingual purity
(defalias 'indent-to-column 'indent-to)
(defalias 'backward-delete-char 'delete-backward-char)
(defalias 'search-forward-regexp (symbol-function 're-search-forward))
(defalias 'search-backward-regexp (symbol-function 're-search-backward))
(defalias 'int-to-string 'number-to-string)
(defalias 'store-match-data 'set-match-data)
(defalias 'chmod 'set-file-modes)
(defalias 'mkdir 'make-directory)
;; These are the XEmacs names:
(defalias 'point-at-eol 'line-end-position)
(defalias 'point-at-bol 'line-beginning-position)

(defalias 'user-original-login-name 'user-login-name)


;;;; Hook manipulation functions.

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its global value.
This makes the hook buffer-local, and it makes t a member of the
buffer-local value.  That acts as a flag to run the hook
functions of the global value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  (if local (unless (local-variable-if-set-p hook)
	      (set (make-local-variable hook) (list t)))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
      (setq local t)))
  (let ((hook-value (if local (symbol-value hook) (default-value hook))))
    ;; If the hook value is a single function, turn it into a list.
    (when (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
      (setq hook-value (list hook-value)))
    ;; Do the actual addition if necessary
    (unless (member function hook-value)
      (when (stringp function)
	(setq function (purecopy function)))
      (setq hook-value
	    (if append
		(append hook-value (list function))
	      (cons function hook-value))))
    ;; Set the actual variable
    (if local
	(progn
	  ;; If HOOK isn't a permanent local,
	  ;; but FUNCTION wants to survive a change of modes,
	  ;; mark HOOK as partially permanent.
	  (and (symbolp function)
	       (get function 'permanent-local-hook)
	       (not (get hook 'permanent-local))
	       (put hook 'permanent-local 'permanent-local-hook))
	  (set hook hook-value))
      (set-default hook hook-value))))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  ;; Do nothing if LOCAL is t but this hook has no local binding.
  (unless (and local (not (local-variable-p hook)))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (when (and (local-variable-p hook)
	       (not (and (consp (symbol-value hook))
			 (memq t (symbol-value hook)))))
      (setq local t))
    (let ((hook-value (if local (symbol-value hook) (default-value hook))))
      ;; Remove the function, for both the list and the non-list cases.
      (if (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
	  (if (equal hook-value function) (setq hook-value nil))
	(setq hook-value (delete function (copy-sequence hook-value))))
      ;; If the function is on the global hook, we need to shadow it locally
      ;;(when (and local (member function (default-value hook))
      ;;	       (not (member (cons 'not function) hook-value)))
      ;;  (push (cons 'not function) hook-value))
      ;; Set the actual variable
      (if (not local)
	  (set-default hook hook-value)
	(if (equal hook-value '(t))
	    (kill-local-variable hook)
	  (set hook hook-value))))))

(defmacro letrec (binders &rest body)
  "Bind variables according to BINDERS then eval BODY.
The value of the last form in BODY is returned.
Each element of BINDERS is a list (SYMBOL VALUEFORM) which binds
SYMBOL to the value of VALUEFORM.
All symbols are bound before the VALUEFORMs are evalled."
  ;; Only useful in lexical-binding mode.
  ;; As a special-form, we could implement it more efficiently (and cleanly,
  ;; making the vars actually unbound during evaluation of the binders).
  (declare (debug let) (indent 1))
  `(let ,(mapcar #'car binders)
     ,@(mapcar (lambda (binder) `(setq ,@binder)) binders)
     ,@body))

(defmacro with-wrapper-hook (hook args &rest body)
  "Run BODY, using wrapper functions from HOOK with additional ARGS.
HOOK is an abnormal hook.  Each hook function in HOOK \"wraps\"
around the preceding ones, like a set of nested `around' advices.

Each hook function should accept an argument list consisting of a
function FUN, followed by the additional arguments in ARGS.

The first hook function in HOOK is passed a FUN that, if it is called
with arguments ARGS, performs BODY (i.e., the default operation).
The FUN passed to each successive hook function is defined based
on the preceding hook functions; if called with arguments ARGS,
it does what the `with-wrapper-hook' call would do if the
preceding hook functions were the only ones present in HOOK.

Each hook function may call its FUN argument as many times as it wishes,
including never.  In that case, such a hook function acts to replace
the default definition altogether, and any preceding hook functions.
Of course, a subsequent hook function may do the same thing.

Each hook function definition is used to construct the FUN passed
to the next hook function, if any.  The last (or \"outermost\")
FUN is then called once."
  (declare (indent 2) (debug (form sexp body)))
  ;; We need those two gensyms because CL's lexical scoping is not available
  ;; for function arguments :-(
  (let ((funs (make-symbol "funs"))
        (global (make-symbol "global"))
        (argssym (make-symbol "args"))
        (runrestofhook (make-symbol "runrestofhook")))
    ;; Since the hook is a wrapper, the loop has to be done via
    ;; recursion: a given hook function will call its parameter in order to
    ;; continue looping.
    `(letrec ((,runrestofhook
               (lambda (,funs ,global ,argssym)
                 ;; `funs' holds the functions left on the hook and `global'
                 ;; holds the functions left on the global part of the hook
                 ;; (in case the hook is local).
                 (if (consp ,funs)
                     (if (eq t (car ,funs))
                         (funcall ,runrestofhook
                                  (append ,global (cdr ,funs)) nil ,argssym)
                       (apply (car ,funs)
                              (apply-partially
                               (lambda (,funs ,global &rest ,argssym)
                                 (funcall ,runrestofhook ,funs ,global ,argssym))
                               (cdr ,funs) ,global)
                              ,argssym))
                   ;; Once there are no more functions on the hook, run
                   ;; the original body.
                   (apply (lambda ,args ,@body) ,argssym)))))
       (funcall ,runrestofhook ,hook
                ;; The global part of the hook, if any.
                ,(if (symbolp hook)
                     `(if (local-variable-p ',hook)
                          (default-value ',hook)))
                (list ,@args)))))

(defun add-to-list (list-var element &optional append compare-fn)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `equal',
or with COMPARE-FN if that's non-nil.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

The return value is the new value of LIST-VAR.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (cond
       ((null compare-fn)
	(member element (symbol-value list-var)))
       ((eq compare-fn 'eq)
	(memq element (symbol-value list-var)))
       ((eq compare-fn 'eql)
	(memql element (symbol-value list-var)))
       (t
	(let ((lst (symbol-value list-var)))
	  (while (and lst
		      (not (funcall compare-fn element (car lst))))
	    (setq lst (cdr lst)))
          lst)))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))


(defun add-to-ordered-list (list-var element &optional order)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `eq'.

The resulting list is reordered so that the elements are in the
order given by each element's numeric list order.  Elements
without a numeric list order are placed at the end of the list.

If the third optional argument ORDER is a number (integer or
float), set the element's list order to the given value.  If
ORDER is nil or omitted, do not change the numeric order of
ELEMENT.  If ORDER has any other value, remove the numeric order
of ELEMENT if it has one.

The list order for each element is stored in LIST-VAR's
`list-order' property.

The return value is the new value of LIST-VAR."
  (let ((ordering (get list-var 'list-order)))
    (unless ordering
      (put list-var 'list-order
           (setq ordering (make-hash-table :weakness 'key :test 'eq))))
    (when order
      (puthash element (and (numberp order) order) ordering))
    (unless (memq element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var))))
    (set list-var (sort (symbol-value list-var)
			(lambda (a b)
			  (let ((oa (gethash a ordering))
				(ob (gethash b ordering)))
			    (if (and oa ob)
				(< oa ob)
			      oa)))))))

(defun add-to-history (history-var newelt &optional maxelt keep-all)
  "Add NEWELT to the history list stored in the variable HISTORY-VAR.
Return the new history list.
If MAXELT is non-nil, it specifies the maximum length of the history.
Otherwise, the maximum history length is the value of the `history-length'
property on symbol HISTORY-VAR, if set, or the value of the `history-length'
variable.
Remove duplicates of NEWELT if `history-delete-duplicates' is non-nil.
If optional fourth arg KEEP-ALL is non-nil, add NEWELT to history even
if it is empty or a duplicate."
  (unless maxelt
    (setq maxelt (or (get history-var 'history-length)
		     history-length)))
  (let ((history (symbol-value history-var))
	tail)
    (when (and (listp history)
	       (or keep-all
		   (not (stringp newelt))
		   (> (length newelt) 0))
	       (or keep-all
		   (not (equal (car history) newelt))))
      (if history-delete-duplicates
	  (delete newelt history))
      (setq history (cons newelt history))
      (when (integerp maxelt)
	(if (= 0 maxelt)
	    (setq history nil)
	  (setq tail (nthcdr (1- maxelt) history))
	  (when (consp tail)
	    (setcdr tail nil)))))
    (set history-var history)))


;;;; Mode hooks.

(defvar delay-mode-hooks nil
  "If non-nil, `run-mode-hooks' should delay running the hooks.")
(defvar delayed-mode-hooks nil
  "List of delayed mode hooks waiting to be run.")
(make-variable-buffer-local 'delayed-mode-hooks)
(put 'delay-mode-hooks 'permanent-local t)

(defvar change-major-mode-after-body-hook nil
  "Normal hook run in major mode functions, before the mode hooks.")

(defvar after-change-major-mode-hook nil
  "Normal hook run at the very end of major mode functions.")

(defun run-mode-hooks (&rest hooks)
  "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
If the variable `delay-mode-hooks' is non-nil, does not run any hooks,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, and finally
`after-change-major-mode-hook'.  Major mode functions should use
this instead of `run-hooks' when running their FOO-mode-hook."
  (if delay-mode-hooks
      ;; Delaying case.
      (dolist (hook hooks)
	(push hook delayed-mode-hooks))
    ;; Normal case, just run the hook as before plus any delayed hooks.
    (setq hooks (nconc (nreverse delayed-mode-hooks) hooks))
    (setq delayed-mode-hooks nil)
    (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
    (run-hooks 'after-change-major-mode-hook)))

(defmacro delay-mode-hooks (&rest body)
  "Execute BODY, but delay any `run-mode-hooks'.
These hooks will be executed by the first following call to
`run-mode-hooks' that occurs outside any `delayed-mode-hooks' form.
Only affects hooks run in the current buffer."
  (declare (debug t) (indent 0))
  `(progn
     (make-local-variable 'delay-mode-hooks)
     (let ((delay-mode-hooks t))
       ,@body)))

;; PUBLIC: find if the current mode derives from another.

(defun derived-mode-p (&rest modes)
  "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
  (let ((parent major-mode))
    (while (and (not (memq parent modes))
		(setq parent (get parent 'derived-mode-parent))))
    parent))

;;;; Minor modes.

;; If a minor mode is not defined with define-minor-mode,
;; add it here explicitly.
;; isearch-mode is deliberately excluded, since you should
;; not call it yourself.
(defvar minor-mode-list '(auto-save-mode auto-fill-mode abbrev-mode
					 overwrite-mode view-mode
                                         hs-minor-mode)
  "List of all minor mode functions.")

(defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Register a new minor mode.

This is an XEmacs-compatibility function.  Use `define-minor-mode' instead.

TOGGLE is a symbol which is the name of a buffer-local variable that
is toggled on or off to say whether the minor mode is active or not.

NAME specifies what will appear in the mode line when the minor mode
is active.  NAME should be either a string starting with a space, or a
symbol whose value is such a string.

Optional KEYMAP is the keymap for the minor mode that will be added
to `minor-mode-map-alist'.

Optional AFTER specifies that TOGGLE should be added after AFTER
in `minor-mode-alist'.

Optional TOGGLE-FUN is an interactive function to toggle the mode.
It defaults to (and should by convention be) TOGGLE.

If TOGGLE has a non-nil `:included' property, an entry for the mode is
included in the mode-line minor mode menu.
If TOGGLE has a `:menu-tag', that is used for the menu item's label."
  (unless (memq toggle minor-mode-list)
    (push toggle minor-mode-list))

  (unless toggle-fun (setq toggle-fun toggle))
  (unless (eq toggle-fun toggle)
    (put toggle :minor-mode-function toggle-fun))
  ;; Add the name to the minor-mode-alist.
  (when name
    (let ((existing (assq toggle minor-mode-alist)))
      (if existing
	  (setcdr existing (list name))
	(let ((tail minor-mode-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (list toggle name)) rest))
	    (push (list toggle name) minor-mode-alist))))))
  ;; Add the toggle to the minor-modes menu if requested.
  (when (get toggle :included)
    (define-key mode-line-mode-menu
      (vector toggle)
      (list 'menu-item
	    (concat
	     (or (get toggle :menu-tag)
		 (if (stringp name) name (symbol-name toggle)))
	     (let ((mode-name (if (symbolp name) (symbol-value name))))
	       (if (and (stringp mode-name) (string-match "[^ ]+" mode-name))
		   (concat " (" (match-string 0 mode-name) ")"))))
	    toggle-fun
	    :button (cons :toggle toggle))))

  ;; Add the map to the minor-mode-map-alist.
  (when keymap
    (let ((existing (assq toggle minor-mode-map-alist)))
      (if existing
	  (setcdr existing keymap)
	(let ((tail minor-mode-map-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (cons toggle keymap)) rest))
	    (push (cons toggle keymap) minor-mode-map-alist)))))))

;;; Load history

(defun symbol-file (symbol &optional type)
  "Return the name of the file that defined SYMBOL.
The value is normally an absolute file name.  It can also be nil,
if the definition is not associated with any file.  If SYMBOL
specifies an autoloaded function, the value can be a relative
file name without extension.

If TYPE is nil, then any kind of definition is acceptable.  If
TYPE is `defun', `defvar', or `defface', that specifies function
definition, variable definition, or face definition only."
  (if (and (or (null type) (eq type 'defun))
	   (symbolp symbol) (fboundp symbol)
	   (eq 'autoload (car-safe (symbol-function symbol))))
      (nth 1 (symbol-function symbol))
    (let ((files load-history)
	  file)
      (while files
	(if (if type
		(if (eq type 'defvar)
		    ;; Variables are present just as their names.
		    (member symbol (cdr (car files)))
		  ;; Other types are represented as (TYPE . NAME).
		  (member (cons type symbol) (cdr (car files))))
	      ;; We accept all types, so look for variable def
	      ;; and then for any other kind.
	      (or (member symbol (cdr (car files)))
		  (rassq symbol (cdr (car files)))))
	    (setq file (car (car files)) files nil))
	(setq files (cdr files)))
      file)))

(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
LIBRARY should be a relative file name of the library, a string.
It can omit the suffix (a.k.a. file-name extension) if NOSUFFIX is
nil (which is the default, see below).
This command searches the directories in `load-path' like `\\[load-library]'
to find the file that `\\[load-library] RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `load-suffixes'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normally returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (completing-read "Locate library: "
				      (apply-partially
                                       'locate-file-completion-table
                                       load-path (get-load-suffixes)))
		     nil nil
		     t))
  (let ((file (locate-file library
			   (or path load-path)
			   (append (unless nosuffix (get-load-suffixes))
				   load-file-rep-suffixes))))
    (if interactive-call
	(if file
	    (message "Library is file %s" (abbreviate-file-name file))
	  (message "No library %s in search path" library)))
    file))


;;;; Specifying things to do later.

(defun load-history-regexp (file)
  "Form a regexp to find FILE in `load-history'.
FILE, a string, is described in the function `eval-after-load'."
  (if (file-name-absolute-p file)
      (setq file (file-truename file)))
  (concat (if (file-name-absolute-p file) "\\`" "\\(\\`\\|/\\)")
	  (regexp-quote file)
	  (if (file-name-extension file)
	      ""
	    ;; Note: regexp-opt can't be used here, since we need to call
	    ;; this before Emacs has been fully started.  2006-05-21
	    (concat "\\(" (mapconcat 'regexp-quote load-suffixes "\\|") "\\)?"))
	  "\\(" (mapconcat 'regexp-quote jka-compr-load-suffixes "\\|")
	  "\\)?\\'"))

(defun load-history-filename-element (file-regexp)
  "Get the first elt of `load-history' whose car matches FILE-REGEXP.
Return nil if there isn't one."
  (let* ((loads load-history)
	 (load-elt (and loads (car loads))))
    (save-match-data
      (while (and loads
		  (or (null (car load-elt))
		      (not (string-match file-regexp (car load-elt)))))
	(setq loads (cdr loads)
	      load-elt (and loads (car loads)))))
    load-elt))

(put 'eval-after-load 'lisp-indent-function 1)
(defun eval-after-load (file form)
  "Arrange that if FILE is loaded, FORM will be run immediately afterwards.
If FILE is already loaded, evaluate FORM right now.

If a matching file is loaded again, FORM will be evaluated again.

If FILE is a string, it may be either an absolute or a relative file
name, and may have an extension \(e.g. \".el\") or may lack one, and
additionally may or may not have an extension denoting a compressed
format \(e.g. \".gz\").

When FILE is absolute, this first converts it to a true name by chasing
symbolic links.  Only a file of this name \(see next paragraph regarding
extensions) will trigger the evaluation of FORM.  When FILE is relative,
a file whose absolute true name ends in FILE will trigger evaluation.

When FILE lacks an extension, a file name with any extension will trigger
evaluation.  Otherwise, its extension must match FILE's.  A further
extension for a compressed format \(e.g. \".gz\") on FILE will not affect
this name matching.

Alternatively, FILE can be a feature (i.e. a symbol), in which case FORM
is evaluated at the end of any file that `provide's this feature.
If the feature is provided when evaluating code not associated with a
file, FORM is evaluated immediately after the provide statement.

Usually FILE is just a library name like \"font-lock\" or a feature name
like 'font-lock.

This function makes or adds to an entry on `after-load-alist'."
  ;; Add this FORM into after-load-alist (regardless of whether we'll be
  ;; evaluating it now).
  (let* ((regexp-or-feature
	  (if (stringp file)
              (setq file (purecopy (load-history-regexp file)))
            file))
	 (elt (assoc regexp-or-feature after-load-alist)))
    (unless elt
      (setq elt (list regexp-or-feature))
      (push elt after-load-alist))
    ;; Make sure `form' is evalled in the current lexical/dynamic code.
    (setq form `(funcall ',(eval `(lambda () ,form) lexical-binding)))
    ;; Is there an already loaded file whose name (or `provide' name)
    ;; matches FILE?
    (prog1 (if (if (stringp file)
		   (load-history-filename-element regexp-or-feature)
		 (featurep file))
	       (eval form))
      (when (symbolp regexp-or-feature)
	;; For features, the after-load-alist elements get run when `provide' is
	;; called rather than at the end of the file.  So add an indirection to
	;; make sure that `form' is really run "after-load" in case the provide
	;; call happens early.
	(setq form
	      `(if load-file-name
		   (let ((fun (make-symbol "eval-after-load-helper")))
		     (fset fun `(lambda (file)
				  (if (not (equal file ',load-file-name))
				      nil
				    (remove-hook 'after-load-functions ',fun)
				    ,',form)))
		     (add-hook 'after-load-functions fun))
		 ;; Not being provided from a file, run form right now.
		 ,form)))
      ;; Add FORM to the element unless it's already there.
      (unless (member form (cdr elt))
	(nconc elt (purecopy (list form)))))))

(defvar after-load-functions nil
  "Special hook run after loading a file.
Each function there is called with a single argument, the absolute
name of the file just loaded.")

(defun do-after-load-evaluation (abs-file)
  "Evaluate all `eval-after-load' forms, if any, for ABS-FILE.
ABS-FILE, a string, should be the absolute true name of a file just loaded.
This function is called directly from the C code."
  ;; Run the relevant eval-after-load forms.
  (mapc #'(lambda (a-l-element)
	    (when (and (stringp (car a-l-element))
		       (string-match-p (car a-l-element) abs-file))
	      ;; discard the file name regexp
	      (mapc #'eval (cdr a-l-element))))
	after-load-alist)
  ;; Complain when the user uses obsolete files.
  (when (string-match-p "/obsolete/[^/]*\\'" abs-file)
    (run-with-timer 0 nil
                    (lambda (file)
                      (message "Package %s is obsolete!"
                               (substring file 0
                                          (string-match "\\.elc?\\>" file))))
                    (file-name-nondirectory abs-file)))
  ;; Finally, run any other hook.
  (run-hook-with-args 'after-load-functions abs-file))

(defun eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))
(make-obsolete 'eval-next-after-load `eval-after-load "23.2")

(defun display-delayed-warnings ()
  "Display delayed warnings from `delayed-warnings-list'.
Used from `delayed-warnings-hook' (which see)."
  (dolist (warning (nreverse delayed-warnings-list))
    (apply 'display-warning warning))
  (setq delayed-warnings-list nil))

(defun collapse-delayed-warnings ()
  "Remove duplicates from `delayed-warnings-list'.
Collapse identical adjacent warnings into one (plus count).
Used from `delayed-warnings-hook' (which see)."
  (let ((count 1)
        collapsed warning)
    (while delayed-warnings-list
      (setq warning (pop delayed-warnings-list))
      (if (equal warning (car delayed-warnings-list))
          (setq count (1+ count))
        (when (> count 1)
          (setcdr warning (cons (format "%s [%d times]" (cadr warning) count)
                                (cddr warning)))
          (setq count 1))
        (push warning collapsed)))
    (setq delayed-warnings-list (nreverse collapsed))))

;; At present this is only used for Emacs internals.
;; Ref http://lists.gnu.org/archive/html/emacs-devel/2012-02/msg00085.html
(defvar delayed-warnings-hook '(collapse-delayed-warnings
                                display-delayed-warnings)
  "Normal hook run to process and display delayed warnings.
By default, this hook contains functions to consolidate the
warnings listed in `delayed-warnings-list', display them, and set
`delayed-warnings-list' back to nil.")


;;;; Process stuff.

(defun process-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
Signal an error if the program returns with a non-zero exit status."
  (with-temp-buffer
    (let ((status (apply 'call-process program nil (current-buffer) nil args)))
      (unless (eq status 0)
	(error "%s exited with status %s" program status))
      (goto-char (point-min))
      (let (lines)
	(while (not (eobp))
	  (setq lines (cons (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))
			    lines))
	  (forward-line 1))
	(nreverse lines)))))

(defun process-live-p (process)
  "Returns non-nil if PROCESS is alive.
A process is considered alive if its status is `run', `open',
`listen', `connect' or `stop'."
  (memq (process-status process)
        '(run open listen connect stop)))

;; compatibility

(make-obsolete
 'process-kill-without-query
 "use `process-query-on-exit-flag' or `set-process-query-on-exit-flag'."
 "22.1")
(defun process-kill-without-query (process &optional _flag)
  "Say no query needed if PROCESS is running when Emacs is exited.
Optional second argument if non-nil says to require a query.
Value is t if a query was formerly required."
  (let ((old (process-query-on-exit-flag process)))
    (set-process-query-on-exit-flag process nil)
    old))

(defun process-kill-buffer-query-function ()
  "Ask before killing a buffer that has a running process."
  (let ((process (get-buffer-process (current-buffer))))
    (or (not process)
        (not (memq (process-status process) '(run stop open listen)))
        (not (process-query-on-exit-flag process))
        (yes-or-no-p
	 (format "Buffer %S has a running process; kill it? "
		 (buffer-name (current-buffer)))))))

(add-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function)

;; process plist management

(defun process-get (process propname)
  "Return the value of PROCESS' PROPNAME property.
This is the last value stored with `(process-put PROCESS PROPNAME VALUE)'."
  (plist-get (process-plist process) propname))

(defun process-put (process propname value)
  "Change PROCESS' PROPNAME property to VALUE.
It can be retrieved with `(process-get PROCESS PROPNAME)'."
  (set-process-plist process
		     (plist-put (process-plist process) propname value)))


;;;; Input and display facilities.

(defvar read-quoted-char-radix 8
  "*Radix for \\[quoted-insert] and other uses of `read-quoted-char'.
Legitimate radix values are 8, 10 and 16.")

(custom-declare-variable-early
 'read-quoted-char-radix 8
 "*Radix for \\[quoted-insert] and other uses of `read-quoted-char'.
Legitimate radix values are 8, 10 and 16."
 :type '(choice (const 8) (const 10) (const 16))
 :group 'editing-basics)

(defconst read-key-empty-map (make-sparse-keymap))

(defvar read-key-delay 0.01) ;Fast enough for 100Hz repeat rate, hopefully.

(defun read-key (&optional prompt)
  "Read a key from the keyboard.
Contrary to `read-event' this will not return a raw event but instead will
obey the input decoding and translations usually done by `read-key-sequence'.
So escape sequences and keyboard encoding are taken into account.
When there's an ambiguity because the key looks like the prefix of
some sort of escape sequence, the ambiguity is resolved via `read-key-delay'."
  (let ((overriding-terminal-local-map read-key-empty-map)
	(overriding-local-map nil)
        (echo-keystrokes 0)
	(old-global-map (current-global-map))
        (timer (run-with-idle-timer
                ;; Wait long enough that Emacs has the time to receive and
                ;; process all the raw events associated with the single-key.
                ;; But don't wait too long, or the user may find the delay
                ;; annoying (or keep hitting more keys which may then get
                ;; lost or misinterpreted).
                ;; This is only relevant for keys which Emacs perceives as
                ;; "prefixes", such as C-x (because of the C-x 8 map in
                ;; key-translate-table and the C-x @ map in function-key-map)
                ;; or ESC (because of terminal escape sequences in
                ;; input-decode-map).
                read-key-delay t
                (lambda ()
                  (let ((keys (this-command-keys-vector)))
                    (unless (zerop (length keys))
                      ;; `keys' is non-empty, so the user has hit at least
                      ;; one key; there's no point waiting any longer, even
                      ;; though read-key-sequence thinks we should wait
                      ;; for more input to decide how to interpret the
                      ;; current input.
                      (throw 'read-key keys)))))))
    (unwind-protect
        (progn
	  (use-global-map
           (let ((map (make-sparse-keymap)))
             ;; Don't hide the menu-bar and tool-bar entries.
             (define-key map [menu-bar] (lookup-key global-map [menu-bar]))
             (define-key map [tool-bar]
	       ;; This hack avoids evaluating the :filter (Bug#9922).
	       (or (cdr (assq 'tool-bar global-map))
		   (lookup-key global-map [tool-bar])))
             map))
	  (aref	(catch 'read-key (read-key-sequence-vector prompt nil t)) 0))
      (cancel-timer timer)
      (use-global-map old-global-map))))

(defun read-quoted-char (&optional prompt)
  "Like `read-char', but do not allow quitting.
Also, if the first character read is an octal digit,
we read any number of octal digits and return the
specified character code.  Any nondigit terminates the sequence.
If the terminator is RET, it is discarded;
any other terminator is used itself as input.

The optional argument PROMPT specifies a string to use to prompt the user.
The variable `read-quoted-char-radix' controls which radix to use
for numeric input."
  (let ((message-log-max nil) done (first t) (code 0) char translated)
    (while (not done)
      (let ((inhibit-quit first)
	    ;; Don't let C-h get the help message--only help function keys.
	    (help-char nil)
	    (help-form
	     "Type the special character you want to use,
or the octal character code.
RET terminates the character code and is discarded;
any other non-digit terminates the character code and is then used as input."))
	(setq char (read-event (and prompt (format "%s-" prompt)) t))
	(if inhibit-quit (setq quit-flag nil)))
      ;; Translate TAB key into control-I ASCII character, and so on.
      ;; Note: `read-char' does it using the `ascii-character' property.
      ;; We should try and use read-key instead.
      (let ((translation (lookup-key local-function-key-map (vector char))))
	(setq translated (if (arrayp translation)
			     (aref translation 0)
			   char)))
      (if (integerp translated)
	  (setq translated (char-resolve-modifiers translated)))
      (cond ((null translated))
	    ((not (integerp translated))
	     (setq unread-command-events (list char)
		   done t))
	    ((/= (logand translated ?\M-\^@) 0)
	     ;; Turn a meta-character into a character with the 0200 bit set.
	     (setq code (logior (logand translated (lognot ?\M-\^@)) 128)
		   done t))
	    ((and (<= ?0 translated)
                  (< translated (+ ?0 (min 10 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix) (- translated ?0)))
	     (and prompt (setq prompt (message "%s %c" prompt translated))))
	    ((and (<= ?a (downcase translated))
		  (< (downcase translated)
                     (+ ?a -10 (min 36 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix)
			   (+ 10 (- (downcase translated) ?a))))
	     (and prompt (setq prompt (message "%s %c" prompt translated))))
	    ((and (not first) (eq translated ?\C-m))
	     (setq done t))
	    ((not first)
	     (setq unread-command-events (list char)
		   done t))
	    (t (setq code translated
		     done t)))
      (setq first nil))
    code))

(defun read-passwd (prompt &optional confirm default)
  "Read a password, prompting with PROMPT, and return it.
If optional CONFIRM is non-nil, read the password twice to make sure.
Optional DEFAULT is a default password to use instead of empty input.

This function echoes `.' for each character that the user types.

The user ends with RET, LFD, or ESC.  DEL or C-h rubs out.
C-y yanks the current kill.  C-u kills line.
C-g quits; if `inhibit-quit' was non-nil around this function,
then it returns nil if the user types C-g, but `quit-flag' remains set.

Once the caller uses the password, it can erase the password
by doing (clear-string STRING)."
  (with-local-quit
    (if confirm
	(let (success)
	  (while (not success)
	    (let ((first (read-passwd prompt nil default))
		  (second (read-passwd "Confirm password: " nil default)))
	      (if (equal first second)
		  (progn
		    (and (arrayp second) (clear-string second))
		    (setq success first))
		(and (arrayp first) (clear-string first))
		(and (arrayp second) (clear-string second))
		(message "Password not repeated accurately; please start over")
		(sit-for 1))))
	  success)
      (let ((pass nil)
	    ;; Copy it so that add-text-properties won't modify
	    ;; the object that was passed in by the caller.
	    (prompt (copy-sequence prompt))
	    (c 0)
	    (echo-keystrokes 0)
	    (cursor-in-echo-area t)
	    (message-log-max nil)
	    (stop-keys (list 'return ?\r ?\n ?\e))
	    (rubout-keys (list 'backspace ?\b ?\177)))
	(add-text-properties 0 (length prompt)
			     minibuffer-prompt-properties prompt)
	(while (progn (message "%s%s"
			       prompt
			       (make-string (length pass) ?.))
		      (setq c (read-key))
		      (not (memq c stop-keys)))
	  (clear-this-command-keys)
	  (cond ((memq c rubout-keys) ; rubout
		 (when (> (length pass) 0)
		   (let ((new-pass (substring pass 0 -1)))
		     (and (arrayp pass) (clear-string pass))
		     (setq pass new-pass))))
                ((eq c ?\C-g) (keyboard-quit))
		((not (numberp c)))
		((= c ?\C-u) ; kill line
		 (and (arrayp pass) (clear-string pass))
		 (setq pass ""))
		((= c ?\C-y) ; yank
		 (let* ((str (condition-case nil
				 (current-kill 0)
			       (error nil)))
			new-pass)
		   (when str
		     (setq new-pass
			   (concat pass
				   (substring-no-properties str)))
		     (and (arrayp pass) (clear-string pass))
		     (setq c ?\0)
		     (setq pass new-pass))))
		((characterp c) ; insert char
		 (let* ((new-char (char-to-string c))
			(new-pass (concat pass new-char)))
		   (and (arrayp pass) (clear-string pass))
		   (clear-string new-char)
		   (setq c ?\0)
		   (setq pass new-pass)))))
	(message nil)
	(or pass default "")))))

;; This should be used by `call-interactively' for `n' specs.
(defun read-number (prompt &optional default)
  "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The value of DEFAULT is inserted into PROMPT."
  (let ((n nil))
    (when default
      (setq prompt
	    (if (string-match "\\(\\):[ \t]*\\'" prompt)
		(replace-match (format " (default %s)" default) t t prompt 1)
	      (replace-regexp-in-string "[ \t]*\\'"
					(format " (default %s) " default)
					prompt t t))))
    (while
	(progn
	  (let ((str (read-from-minibuffer prompt nil nil nil nil
					   (and default
						(number-to-string default)))))
	    (condition-case nil
		(setq n (cond
			 ((zerop (length str)) default)
			 ((stringp str) (read str))))
	      (error nil)))
	  (unless (numberp n)
	    (message "Please enter a number.")
	    (sit-for 1)
	    t)))
    n))

(defun read-char-choice (prompt chars &optional inhibit-keyboard-quit)
  "Read and return one of CHARS, prompting for PROMPT.
Any input that is not one of CHARS is ignored.

If optional argument INHIBIT-KEYBOARD-QUIT is non-nil, ignore
keyboard-quit events while waiting for a valid input."
  (unless (consp chars)
    (error "Called `read-char-choice' without valid char choices"))
  (let (char done show-help (helpbuf " *Char Help*"))
    (let ((cursor-in-echo-area t)
          (executing-kbd-macro executing-kbd-macro))
      (save-window-excursion	      ; in case we call help-form-show
	(while (not done)
	  (unless (get-text-property 0 'face prompt)
	    (setq prompt (propertize prompt 'face 'minibuffer-prompt)))
	  (setq char (let ((inhibit-quit inhibit-keyboard-quit))
		       (read-key prompt)))
	  (and show-help (buffer-live-p (get-buffer helpbuf))
	       (kill-buffer helpbuf))
	  (cond
	   ((not (numberp char)))
	   ;; If caller has set help-form, that's enough.
	   ;; They don't explicitly have to add help-char to chars.
	   ((and help-form
		 (eq char help-char)
		 (setq show-help t)
		 (help-form-show)))
	   ((memq char chars)
	    (setq done t))
	   ((and executing-kbd-macro (= char -1))
	    ;; read-event returns -1 if we are in a kbd macro and
	    ;; there are no more events in the macro.  Attempt to
	    ;; get an event interactively.
	    (setq executing-kbd-macro nil))
	   ((and (not inhibit-keyboard-quit) (eq char ?\C-g))
	    (keyboard-quit))))))
    ;; Display the question with the answer.  But without cursor-in-echo-area.
    (message "%s%s" prompt (char-to-string char))
    char))

(defun sit-for (seconds &optional nodisp obsolete)
  "Perform redisplay, then wait for SECONDS seconds or until input is available.
SECONDS may be a floating-point value.
\(On operating systems that do not support waiting for fractions of a
second, floating-point values are rounded down to the nearest integer.)

If optional arg NODISP is t, don't redisplay, just wait for input.
Redisplay does not happen if input is available before it starts.

Value is t if waited the full time with no input arriving, and nil otherwise.

An obsolete, but still supported form is
\(sit-for SECONDS &optional MILLISECONDS NODISP)
where the optional arg MILLISECONDS specifies an additional wait period,
in milliseconds; this was useful when Emacs was built without
floating point support."
  (if (numberp nodisp)
      (setq seconds (+ seconds (* 1e-3 nodisp))
            nodisp obsolete)
    (if obsolete (setq nodisp obsolete)))
  (cond
   (noninteractive
    (sleep-for seconds)
    t)
   ((input-pending-p)
    nil)
   ((<= seconds 0)
    (or nodisp (redisplay)))
   (t
    (or nodisp (redisplay))
    (let ((read (read-event nil nil seconds)))
      (or (null read)
	  (progn
	    ;; If last command was a prefix arg, e.g. C-u, push this event onto
	    ;; unread-command-events as (t . EVENT) so it will be added to
	    ;; this-command-keys by read-key-sequence.
	    (if (eq overriding-terminal-local-map universal-argument-map)
		(setq read (cons t read)))
	    (push read unread-command-events)
	    nil))))))
(set-advertised-calling-convention 'sit-for '(seconds &optional nodisp) "22.1")

(defun y-or-n-p (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
PROMPT is the string to display to ask the question.  It should
end in a space; `y-or-n-p' adds \"(y or n) \" to it.

No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no.  \(Actually, it uses
the bindings in `query-replace-map'; see the documentation of that variable
for more information.  In this case, the useful bindings are `act', `skip',
`recenter', and `quit'.\)

Under a windowing system a dialog box will be used if `last-nonmenu-event'
is nil and `use-dialog-box' is non-nil."
  ;; ¡Beware! when I tried to edebug this code, Emacs got into a weird state
  ;; where all the keys were unbound (i.e. it somehow got triggered
  ;; within read-key, apparently).  I had to kill it.
  (let ((answer 'recenter))
    (cond
     (noninteractive
      (setq prompt (concat prompt
                           (if (eq ?\s (aref prompt (1- (length prompt))))
                               "" " ")
                           "(y or n) "))
      (let ((temp-prompt prompt))
	(while (not (memq answer '(act skip)))
	  (let ((str (read-string temp-prompt)))
	    (cond ((member str '("y" "Y")) (setq answer 'act))
		  ((member str '("n" "N")) (setq answer 'skip))
		  (t (setq temp-prompt (concat "Please answer y or n.  "
					       prompt))))))))
     ((and (display-popup-menus-p)
	   (listp last-nonmenu-event)
	   use-dialog-box)
      (setq answer
	    (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
     (t
      (setq prompt (concat prompt
                           (if (eq ?\s (aref prompt (1- (length prompt))))
                               "" " ")
                           "(y or n) "))
      (while
          (let* ((key
                  (let ((cursor-in-echo-area t))
                    (when minibuffer-auto-raise
                      (raise-frame (window-frame (minibuffer-window))))
                    (read-key (propertize (if (eq answer 'recenter)
                                              prompt
                                            (concat "Please answer y or n.  "
                                                    prompt))
                                          'face 'minibuffer-prompt)))))
            (setq answer (lookup-key query-replace-map (vector key) t))
            (cond
             ((memq answer '(skip act)) nil)
             ((eq answer 'recenter) (recenter) t)
             ((memq answer '(exit-prefix quit)) (signal 'quit nil) t)
             (t t)))
        (ding)
        (discard-input))))
    (let ((ret (eq answer 'act)))
      (unless noninteractive
        ;; FIXME this prints one too many spaces, since prompt
        ;; already ends in a space.  Eg "... (y or n)  y".
        (message "%s %s" prompt (if ret "y" "n")))
      ret)))


;;; Atomic change groups.

(defmacro atomic-change-group (&rest body)
  "Perform BODY as an atomic change group.
This means that if BODY exits abnormally,
all of its changes to the current buffer are undone.
This works regardless of whether undo is enabled in the buffer.

This mechanism is transparent to ordinary use of undo;
if undo is enabled in the buffer and BODY succeeds, the
user can undo the change normally."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
	(success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
	   ;; Don't truncate any undo data in the middle of this.
	   (undo-outer-limit nil)
	   (undo-limit most-positive-fixnum)
	   (undo-strong-limit most-positive-fixnum)
	   (,success nil))
       (unwind-protect
	   (progn
	     ;; This is inside the unwind-protect because
	     ;; it enables undo if that was disabled; we need
	     ;; to make sure that it gets disabled again.
	     (activate-change-group ,handle)
	     ,@body
	     (setq ,success t))
	 ;; Either of these functions will disable undo
	 ;; if it was disabled before.
	 (if ,success
	     (accept-change-group ,handle)
	   (cancel-change-group ,handle))))))

(defun prepare-change-group (&optional buffer)
  "Return a handle for the current buffer's state, for a change group.
If you specify BUFFER, make a handle for BUFFER's state instead.

Pass the handle to `activate-change-group' afterward to initiate
the actual changes of the change group.

To finish the change group, call either `accept-change-group' or
`cancel-change-group' passing the same handle as argument.  Call
`accept-change-group' to accept the changes in the group as final;
call `cancel-change-group' to undo them all.  You should use
`unwind-protect' to make sure the group is always finished.  The call
to `activate-change-group' should be inside the `unwind-protect'.
Once you finish the group, don't use the handle again--don't try to
finish the same group twice.  For a simple example of correct use, see
the source code of `atomic-change-group'.

The handle records only the specified buffer.  To make a multibuffer
change group, call this function once for each buffer you want to
cover, then use `nconc' to combine the returned values, like this:

  (nconc (prepare-change-group buffer-1)
         (prepare-change-group buffer-2))

You can then activate that multibuffer change group with a single
call to `activate-change-group' and finish it with a single call
to `accept-change-group' or `cancel-change-group'."

  (if buffer
      (list (cons buffer (with-current-buffer buffer buffer-undo-list)))
    (list (cons (current-buffer) buffer-undo-list))))

(defun activate-change-group (handle)
  "Activate a change group made with `prepare-change-group' (which see)."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (if (eq buffer-undo-list t)
	  (setq buffer-undo-list nil)))))

(defun accept-change-group (handle)
  "Finish a change group made with `prepare-change-group' (which see).
This finishes the change group by accepting its changes as final."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (if (eq elt t)
	  (setq buffer-undo-list t)))))

(defun cancel-change-group (handle)
  "Finish a change group made with `prepare-change-group' (which see).
This finishes the change group by reverting all of its changes."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (setq elt (cdr elt))
      (save-restriction
	;; Widen buffer temporarily so if the buffer was narrowed within
	;; the body of `atomic-change-group' all changes can be undone.
	(widen)
	(let ((old-car
	       (if (consp elt) (car elt)))
	      (old-cdr
	       (if (consp elt) (cdr elt))))
	  ;; Temporarily truncate the undo log at ELT.
	  (when (consp elt)
	    (setcar elt nil) (setcdr elt nil))
	  (unless (eq last-command 'undo) (undo-start))
	  ;; Make sure there's no confusion.
	  (when (and (consp elt) (not (eq elt (last pending-undo-list))))
	    (error "Undoing to some unrelated state"))
	  ;; Undo it all.
	  (save-excursion
	    (while (listp pending-undo-list) (undo-more 1)))
	  ;; Reset the modified cons cell ELT to its original content.
	  (when (consp elt)
	    (setcar elt old-car)
	    (setcdr elt old-cdr))
	  ;; Revert the undo info to what it was when we grabbed the state.
	  (setq buffer-undo-list elt))))))

;;;; Display-related functions.

;; For compatibility.
(defalias 'redraw-modeline 'force-mode-line-update)

(defun force-mode-line-update (&optional all)
  "Force redisplay of the current buffer's mode line and header line.
With optional non-nil ALL, force redisplay of all mode lines and
header lines.  This function also forces recomputation of the
menu bar menus and the frame title."
  (if all (with-current-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun momentary-string-display (string pos &optional exit-char message)
  "Momentarily display STRING in the buffer at POS.
Display remains until next event is input.
If POS is a marker, only its position is used; its buffer is ignored.
Optional third arg EXIT-CHAR can be a character, event or event
description list.  EXIT-CHAR defaults to SPC.  If the input is
EXIT-CHAR it is swallowed; otherwise it is then available as
input (as a command if nothing else).
Display MESSAGE (optional fourth arg) in the echo area.
If MESSAGE is nil, instructions to type EXIT-CHAR are displayed there."
  (or exit-char (setq exit-char ?\s))
  (let ((ol (make-overlay pos pos))
        (str (copy-sequence string)))
    (unwind-protect
        (progn
          (save-excursion
            (overlay-put ol 'after-string str)
            (goto-char pos)
            ;; To avoid trouble with out-of-bounds position
            (setq pos (point))
            ;; If the string end is off screen, recenter now.
            (if (<= (window-end nil t) pos)
                (recenter (/ (window-height) 2))))
          (message (or message "Type %s to continue editing.")
                   (single-key-description exit-char))
	  (let ((event (read-event)))
	    ;; `exit-char' can be an event, or an event description list.
	    (or (eq event exit-char)
		(eq event (event-convert-list exit-char))
		(setq unread-command-events (list event)))))
      (delete-overlay ol))))


;;;; Overlay operations

(defun copy-overlay (o)
  "Return a copy of overlay O."
  (let ((o1 (if (overlay-buffer o)
                (make-overlay (overlay-start o) (overlay-end o)
                              ;; FIXME: there's no easy way to find the
                              ;; insertion-type of the two markers.
                              (overlay-buffer o))
              (let ((o1 (make-overlay (point-min) (point-min))))
                (delete-overlay o1)
                o1)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (pop props) (pop props)))
    o1))

(defun remove-overlays (&optional beg end name val)
  "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
  ;; This speeds up the loops over overlays.
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (overlay-recenter end)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (dolist (o (overlays-in beg end))
      (when (eq (overlay-get o name) val)
	;; Either push this overlay outside beg...end
	;; or split it to exclude beg...end
	;; or delete it entirely (if it is contained in beg...end).
	(if (< (overlay-start o) beg)
	    (if (> (overlay-end o) end)
		(progn
		  (move-overlay (copy-overlay o)
				(overlay-start o) beg)
		  (move-overlay o end (overlay-end o)))
	      (move-overlay o (overlay-start o) beg))
	  (if (> (overlay-end o) end)
	      (move-overlay o end (overlay-end o))
	    (delete-overlay o)))))))

;;;; Miscellanea.

(defvar suspend-hook nil
  "Normal hook run by `suspend-emacs', before suspending.")

(defvar suspend-resume-hook nil
  "Normal hook run by `suspend-emacs', after Emacs is continued.")

(defvar temp-buffer-show-hook nil
  "Normal hook run by `with-output-to-temp-buffer' after displaying the buffer.
When the hook runs, the temporary buffer is current, and the window it
was displayed in is selected.")

(defvar temp-buffer-setup-hook nil
  "Normal hook run by `with-output-to-temp-buffer' at the start.
When the hook runs, the temporary buffer is current.
This hook is normally set up with a function to put the buffer in Help
mode.")

;; Avoid compiler warnings about this variable,
;; which has a special meaning on certain system types.
(defvar buffer-file-type nil
  "Non-nil if the visited file is a binary file.
This variable is meaningful on MS-DOG and Windows NT.
On those systems, it is automatically local in every buffer.
On other systems, this variable is normally always nil.")

;; The `assert' macro from the cl package signals
;; `cl-assertion-failed' at runtime so always define it.
(put 'cl-assertion-failed 'error-conditions '(error))
(put 'cl-assertion-failed 'error-message (purecopy "Assertion failed"))

(defconst user-emacs-directory
  (if (eq system-type 'ms-dos)
      ;; MS-DOS cannot have initial dot.
      "~/_emacs.d/"
    "~/.emacs.d/")
  "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator.
See also `locate-user-emacs-file'.")

(defun locate-user-emacs-file (new-name &optional old-name)
  "Return an absolute per-user Emacs-specific file name.
If OLD-NAME is non-nil and ~/OLD-NAME exists, return ~/OLD-NAME.
Else return NEW-NAME in `user-emacs-directory', creating the
directory if it does not exist."
  (convert-standard-filename
   (let* ((home (concat "~" (or init-file-user "")))
	  (at-home (and old-name (expand-file-name old-name home))))
     (if (and at-home (file-readable-p at-home))
	 at-home
       ;; Make sure `user-emacs-directory' exists,
       ;; unless we're in batch mode or dumping Emacs
       (or noninteractive
	   purify-flag
	   (file-accessible-directory-p
	    (directory-file-name user-emacs-directory))
	   (let ((umask (default-file-modes)))
	     (unwind-protect
		 (progn
		   (set-default-file-modes ?\700)
		   (make-directory user-emacs-directory))
	       (set-default-file-modes umask))))
       (abbreviate-file-name
        (expand-file-name new-name user-emacs-directory))))))

;;;; Misc. useful functions.

(defun find-tag-default ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let (from to bound)
    (when (or (progn
		;; Look at text around `point'.
		(save-excursion
		  (skip-syntax-backward "w_") (setq from (point)))
		(save-excursion
		  (skip-syntax-forward "w_") (setq to (point)))
		(> to from))
	      ;; Look between `line-beginning-position' and `point'.
	      (save-excursion
		(and (setq bound (line-beginning-position))
		     (skip-syntax-backward "^w_" bound)
		     (> (setq to (point)) bound)
		     (skip-syntax-backward "w_")
		     (setq from (point))))
	      ;; Look between `point' and `line-end-position'.
	      (save-excursion
		(and (setq bound (line-end-position))
		     (skip-syntax-forward "^w_" bound)
		     (< (setq from (point)) bound)
		     (skip-syntax-forward "w_")
		     (setq to (point)))))
      (buffer-substring-no-properties from to))))

(defun play-sound (sound)
  "SOUND is a list of the form `(sound KEYWORD VALUE...)'.
The following keywords are recognized:

  :file FILE - read sound data from FILE.  If FILE isn't an
absolute file name, it is searched in `data-directory'.

  :data DATA - read sound data from string DATA.

Exactly one of :file or :data must be present.

  :volume VOL - set volume to VOL.  VOL must an integer in the
range 0..100 or a float in the range 0..1.0.  If not specified,
don't change the volume setting of the sound device.

  :device DEVICE - play sound on DEVICE.  If not specified,
a system-dependent default device name is used.

Note: :data and :device are currently not supported on Windows."
  (if (fboundp 'play-sound-internal)
      (play-sound-internal sound)
    (error "This Emacs binary lacks sound support")))

(declare-function w32-shell-dos-semantics "w32-fns" nil)

(defun shell-quote-argument (argument)
  "Quote ARGUMENT for passing as argument to an inferior shell."
  (cond
   ((eq system-type 'ms-dos)
    ;; Quote using double quotes, but escape any existing quotes in
    ;; the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (if (or (null (string-match "[^\"]" argument))
              (< (match-end 0) (length argument)))
          (while (string-match "[\"]" argument start)
            (setq end (match-beginning 0)
                  result (concat result (substring argument start end)
                                 "\\" (substring argument end (1+ end)))
                  start (1+ end))))
      (concat "\"" result (substring argument start) "\"")))

   ((and (eq system-type 'windows-nt) (w32-shell-dos-semantics))

    ;; First, quote argument so that CommandLineToArgvW will
    ;; understand it.  See
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft%28v=vs.85%29.aspx
    ;; After we perform that level of quoting, escape shell
    ;; metacharacters so that cmd won't mangle our argument.  If the
    ;; argument contains no double quote characters, we can just
    ;; surround it with double quotes.  Otherwise, we need to prefix
    ;; each shell metacharacter with a caret.

    (setq argument
          ;; escape backslashes at end of string
          (replace-regexp-in-string
           "\\(\\\\*\\)$"
           "\\1\\1"
           ;; escape backslashes and quotes in string body
           (replace-regexp-in-string
            "\\(\\\\*\\)\""
            "\\1\\1\\\\\""
            argument)))

    (if (string-match "[%!\"]" argument)
        (concat
         "^\""
         (replace-regexp-in-string
          "\\([%!()\"<>&|^]\\)"
          "^\\1"
          argument)
         "^\"")
      (concat "\"" argument "\"")))

   (t
    (if (equal argument "")
        "''"
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (replace-regexp-in-string
       "\n" "'\n'"
       (replace-regexp-in-string "[^-0-9a-zA-Z_./\n]" "\\\\\\&" argument))))
   ))

(defun string-or-null-p (object)
  "Return t if OBJECT is a string or nil.
Otherwise, return nil."
  (or (stringp object) (null object)))

(defun booleanp (object)
  "Return t if OBJECT is one of the two canonical boolean values: t or nil.
Otherwise, return nil."
  (and (memq object '(nil t)) t))

(defun field-at-pos (pos)
  "Return the field at position POS, taking stickiness etc into account."
  (let ((raw-field (get-char-property (field-beginning pos) 'field)))
    (if (eq raw-field 'boundary)
	(get-char-property (1- (field-end pos)) 'field)
      raw-field)))

(defun sha1 (object &optional start end binary)
  "Return the SHA1 (Secure Hash Algorithm) of an OBJECT.
OBJECT is either a string or a buffer.  Optional arguments START and
END are character positions specifying which portion of OBJECT for
computing the hash.  If BINARY is non-nil, return a string in binary
form."
  (secure-hash 'sha1 object start end binary))


;;;; Support for yanking and text properties.

(defvar yank-excluded-properties)

(defun remove-yank-excluded-properties (start end)
  "Remove `yank-excluded-properties' between START and END positions.
Replaces `category' properties with their defined properties."
  (let ((inhibit-read-only t))
    ;; Replace any `category' property with the properties it stands
    ;; for.  This is to remove `mouse-face' properties that are placed
    ;; on categories in *Help* buffers' buttons.  See
    ;; http://lists.gnu.org/archive/html/emacs-devel/2002-04/msg00648.html
    ;; for the details.
    (unless (memq yank-excluded-properties '(t nil))
      (save-excursion
	(goto-char start)
	(while (< (point) end)
	  (let ((cat (get-text-property (point) 'category))
		run-end)
	    (setq run-end
		  (next-single-property-change (point) 'category nil end))
	    (when cat
	      (let (run-end2 original)
		(remove-list-of-text-properties (point) run-end '(category))
		(while (< (point) run-end)
		  (setq run-end2 (next-property-change (point) nil run-end))
		  (setq original (text-properties-at (point)))
		  (set-text-properties (point) run-end2 (symbol-plist cat))
		  (add-text-properties (point) run-end2 original)
		  (goto-char run-end2))))
	    (goto-char run-end)))))
    (if (eq yank-excluded-properties t)
	(set-text-properties start end nil)
      (remove-list-of-text-properties start end yank-excluded-properties))))

(defvar yank-undo-function)

(defun insert-for-yank (string)
  "Call `insert-for-yank-1' repetitively for each `yank-handler' segment.

See `insert-for-yank-1' for more details."
  (let (to)
    (while (setq to (next-single-property-change 0 'yank-handler string))
      (insert-for-yank-1 (substring string 0 to))
      (setq string (substring string to))))
  (insert-for-yank-1 string))

(defun insert-for-yank-1 (string)
  "Insert STRING at point, stripping some text properties.

Strip text properties from the inserted text according to
`yank-excluded-properties'.  Otherwise just like (insert STRING).

If STRING has a non-nil `yank-handler' property on the first character,
the normal insert behavior is modified in various ways.  The value of
the yank-handler property must be a list with one to four elements
with the following format:  (FUNCTION PARAM NOEXCLUDE UNDO).
When FUNCTION is present and non-nil, it is called instead of `insert'
 to insert the string.  FUNCTION takes one argument--the object to insert.
If PARAM is present and non-nil, it replaces STRING as the object
 passed to FUNCTION (or `insert'); for example, if FUNCTION is
 `yank-rectangle', PARAM may be a list of strings to insert as a
 rectangle.
If NOEXCLUDE is present and non-nil, the normal removal of the
 `yank-excluded-properties' is not performed; instead FUNCTION is
 responsible for removing those properties.  This may be necessary
 if FUNCTION adjusts point before or after inserting the object.
If UNDO is present and non-nil, it is a function that will be called
 by `yank-pop' to undo the insertion of the current object.  It is
 called with two arguments, the start and end of the current region.
 FUNCTION may set `yank-undo-function' to override the UNDO value."
  (let* ((handler (and (stringp string)
		       (get-text-property 0 'yank-handler string)))
	 (param (or (nth 1 handler) string))
	 (opoint (point))
	 (inhibit-read-only inhibit-read-only)
	 end)

    (setq yank-undo-function t)
    (if (nth 0 handler) ;; FUNCTION
	(funcall (car handler) param)
      (insert param))
    (setq end (point))

    ;; Prevent read-only properties from interfering with the
    ;; following text property changes.
    (setq inhibit-read-only t)

    ;; What should we do with `font-lock-face' properties?
    (if font-lock-defaults
	;; No, just wipe them.
	(remove-list-of-text-properties opoint end '(font-lock-face))
      ;; Convert them to `face'.
      (save-excursion
	(goto-char opoint)
	(while (< (point) end)
	  (let ((face (get-text-property (point) 'font-lock-face))
		run-end)
	    (setq run-end
		  (next-single-property-change (point) 'font-lock-face nil end))
	    (when face
	      (remove-text-properties (point) run-end '(font-lock-face nil))
	      (put-text-property (point) run-end 'face face))
	    (goto-char run-end)))))

    (unless (nth 2 handler) ;; NOEXCLUDE
      (remove-yank-excluded-properties opoint (point)))

    ;; If last inserted char has properties, mark them as rear-nonsticky.
    (if (and (> end opoint)
	     (text-properties-at (1- end)))
	(put-text-property (1- end) end 'rear-nonsticky t))

    (if (eq yank-undo-function t)		   ;; not set by FUNCTION
	(setq yank-undo-function (nth 3 handler))) ;; UNDO
    (if (nth 4 handler)				   ;; COMMAND
	(setq this-command (nth 4 handler)))))

(defun insert-buffer-substring-no-properties (buffer &optional start end)
  "Insert before point a substring of BUFFER, without text properties.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER."
  (let ((opoint (point)))
    (insert-buffer-substring buffer start end)
    (let ((inhibit-read-only t))
      (set-text-properties opoint (point) nil))))

(defun insert-buffer-substring-as-yank (buffer &optional start end)
  "Insert before point a part of BUFFER, stripping some text properties.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER.
Strip text properties from the inserted text according to
`yank-excluded-properties'."
  ;; Since the buffer text should not normally have yank-handler properties,
  ;; there is no need to handle them here.
  (let ((opoint (point)))
    (insert-buffer-substring buffer start end)
    (remove-yank-excluded-properties opoint (point))))


;;;; Synchronous shell commands.

(defun start-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
COMMAND is the shell command to run.

An old calling convention accepted any number of arguments after COMMAND,
which were just concatenated to COMMAND.  This is still supported but strongly
discouraged."
   ;; We used to use `exec' to replace the shell with the command,
   ;; but that failed to handle (...) and semicolon, etc.
  (start-process name buffer shell-file-name shell-command-switch
		 (mapconcat 'identity args " ")))
(set-advertised-calling-convention 'start-process-shell-command
                                   '(name buffer command) "23.1")

(defun start-file-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Similar to `start-process-shell-command', but calls `start-file-process'."
  (start-file-process
   name buffer
   (if (file-remote-p default-directory) "/bin/sh" shell-file-name)
   (if (file-remote-p default-directory) "-c" shell-command-switch)
   (mapconcat 'identity args " ")))
(set-advertised-calling-convention 'start-file-process-shell-command
                                   '(name buffer command) "23.1")

(defun call-process-shell-command (command &optional infile buffer display
					   &rest args)
  "Execute the shell command COMMAND synchronously in separate process.
The remaining arguments are optional.
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as additional arguments for COMMAND.
Wildcards and redirection are handled as usual in the shell.

If BUFFER is 0, `call-process-shell-command' returns immediately with value nil.
Otherwise it waits for COMMAND to terminate and returns a numeric exit
status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again."
  ;; We used to use `exec' to replace the shell with the command,
  ;; but that failed to handle (...) and semicolon, etc.
  (call-process shell-file-name
		infile buffer display
		shell-command-switch
		(mapconcat 'identity (cons command args) " ")))

(defun process-file-shell-command (command &optional infile buffer display
					   &rest args)
  "Process files synchronously in a separate process.
Similar to `call-process-shell-command', but calls `process-file'."
  (process-file
   (if (file-remote-p default-directory) "/bin/sh" shell-file-name)
   infile buffer display
   (if (file-remote-p default-directory) "-c" shell-command-switch)
   (mapconcat 'identity (cons command args) " ")))

;;;; Lisp macros to do various things temporarily.

(defmacro with-current-buffer (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
The value returned is the value of the last form in BODY.  See
also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  `(save-current-buffer
     (set-buffer ,buffer-or-name)
     ,@body))

(defmacro with-selected-window (window &rest body)
  "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected window, as well as the
selected window of each frame.  It does not change the order of
recently selected windows.  If the previously selected window of
some frame is no longer live at the end of BODY, that frame's
selected window is left alone.  If the selected window is no
longer live, then whatever window is selected at the end of BODY
remains selected.

This macro uses `save-current-buffer' to save and restore the
current buffer, since otherwise its normal operation could
potentially make a different buffer current.  It does not alter
the buffer list ordering."
  (declare (indent 1) (debug t))
  ;; Most of this code is a copy of save-selected-window.
  `(let ((save-selected-window-window (selected-window))
	 ;; It is necessary to save all of these, because calling
	 ;; select-window changes frame-selected-window for whatever
	 ;; frame that window is in.
	 (save-selected-window-alist
	  (mapcar (lambda (frame) (list frame (frame-selected-window frame)))
		  (frame-list))))
     (save-current-buffer
       (unwind-protect
	   (progn (select-window ,window 'norecord)
		  ,@body)
	 (dolist (elt save-selected-window-alist)
	   (and (frame-live-p (car elt))
		(window-live-p (cadr elt))
		(set-frame-selected-window (car elt) (cadr elt) 'norecord)))
	 (when (window-live-p save-selected-window-window)
	   (select-window save-selected-window-window 'norecord))))))

(defmacro with-selected-frame (frame &rest body)
  "Execute the forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected frame, and changes the
order of neither the recently selected windows nor the buffers in
the buffer list."
  (declare (indent 1) (debug t))
  (let ((old-frame (make-symbol "old-frame"))
	(old-buffer (make-symbol "old-buffer")))
    `(let ((,old-frame (selected-frame))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn (select-frame ,frame 'norecord)
		  ,@body)
	 (when (frame-live-p ,old-frame)
	   (select-frame ,old-frame 'norecord))
	 (when (buffer-live-p ,old-buffer)
	   (set-buffer ,old-buffer))))))

(defmacro save-window-excursion (&rest body)
  "Execute BODY, then restore previous window configuration.
This macro saves the window configuration on the selected frame,
executes BODY, then calls `set-window-configuration' to restore
the saved window configuration.  The return value is the last
form in BODY.  The window configuration is also restored if BODY
exits nonlocally.

BEWARE: Most uses of this macro introduce bugs.
E.g. it should not be used to try and prevent some code from opening
a new window, since that window may sometimes appear in another frame,
in which case `save-window-excursion' cannot help."
  (declare (indent 0) (debug t))
  (let ((c (make-symbol "wconfig")))
    `(let ((,c (current-window-configuration)))
       (unwind-protect (progn ,@body)
         (set-window-configuration ,c)))))

(defmacro with-output-to-temp-buffer (bufname &rest body)
  "Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.

This construct makes buffer BUFNAME empty before running BODY.
It does not make the buffer current for BODY.
Instead it binds `standard-output' to that buffer, so that output
generated with `prin1' and similar functions in BODY goes into
the buffer.

At the end of BODY, this marks buffer BUFNAME unmodified and displays
it in a window, but does not select it.  The normal way to do this is
by calling `display-buffer', then running `temp-buffer-show-hook'.
However, if `temp-buffer-show-function' is non-nil, it calls that
function instead (and does not run `temp-buffer-show-hook').  The
function gets one argument, the buffer to display.

The return value of `with-output-to-temp-buffer' is the value of the
last form in BODY.  If BODY does not finish normally, the buffer
BUFNAME is not displayed.

This runs the hook `temp-buffer-setup-hook' before BODY,
with the buffer BUFNAME temporarily current.  It runs the hook
`temp-buffer-show-hook' after displaying buffer BUFNAME, with that
buffer temporarily current, and the window that was used to display it
temporarily selected.  But it doesn't run `temp-buffer-show-hook'
if it uses `temp-buffer-show-function'."
  (declare (debug t))
  (let ((old-dir (make-symbol "old-dir"))
        (buf (make-symbol "buf")))
    `(let* ((,old-dir default-directory)
            (,buf
             (with-current-buffer (get-buffer-create ,bufname)
               (prog1 (current-buffer)
                 (kill-all-local-variables)
                 ;; FIXME: delete_all_overlays
                 (setq default-directory ,old-dir)
                 (setq buffer-read-only nil)
                 (setq buffer-file-name nil)
                 (setq buffer-undo-list t)
                 (let ((inhibit-read-only t)
                       (inhibit-modification-hooks t))
                   (erase-buffer)
                   (run-hooks 'temp-buffer-setup-hook)))))
            (standard-output ,buf))
       (prog1 (progn ,@body)
         (internal-temp-output-buffer-show ,buf)))))

(defmacro with-temp-file (file &rest body)
  "Create a new buffer, evaluate BODY there, and write the buffer to FILE.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  (let ((temp-file (make-symbol "temp-file"))
	(temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-file ,file)
	   (,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp file*"))))
       (unwind-protect
	   (prog1
	       (with-current-buffer ,temp-buffer
		 ,@body)
	     (with-current-buffer ,temp-buffer
	       (write-region nil nil ,temp-file nil 0)))
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defmacro with-temp-message (message &rest body)
  "Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area."
  (declare (debug t) (indent 1))
  (let ((current-message (make-symbol "current-message"))
	(temp-message (make-symbol "with-temp-message")))
    `(let ((,temp-message ,message)
	   (,current-message))
       (unwind-protect
	   (progn
	     (when ,temp-message
	       (setq ,current-message (current-message))
	       (message "%s" ,temp-message))
	     ,@body)
	 (and ,temp-message
	      (if ,current-message
		  (message "%s" ,current-message)
		(message nil)))))))

(defmacro with-temp-buffer (&rest body)
  "Create a temporary buffer, and evaluate BODY there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       ;; FIXME: kill-buffer can change current-buffer in some odd cases.
       (with-current-buffer ,temp-buffer
         (unwind-protect
	     (progn ,@body)
           (and (buffer-name ,temp-buffer)
                (kill-buffer ,temp-buffer)))))))

(defmacro with-silent-modifications (&rest body)
  "Execute BODY, pretending it does not modify the buffer.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.

This macro will run BODY normally, but doesn't count its buffer
modifications as being buffer modifications.  This affects things
like buffer-modified-p, checking whether the file is locked by
someone else, running buffer modification hooks, and other things
of that nature.

Typically used around modifications of text-properties which do
not really affect the buffer's content."
  (declare (debug t) (indent 0))
  (let ((modified (make-symbol "modified")))
    `(let* ((,modified (buffer-modified-p))
            (buffer-undo-list t)
            (inhibit-read-only t)
            (inhibit-modification-hooks t)
            deactivate-mark
            ;; Avoid setting and removing file locks and checking
            ;; buffer's uptodate-ness w.r.t the underlying file.
            buffer-file-name
            buffer-file-truename)
       (unwind-protect
           (progn
             ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(defmacro with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  (declare (indent 0) (debug t))
  `(let ((standard-output
	  (get-buffer-create (generate-new-buffer-name " *string-output*"))))
     (unwind-protect
	 (progn
	   (let ((standard-output standard-output))
	     ,@body)
	   (with-current-buffer standard-output
	     (buffer-string)))
       (kill-buffer standard-output))))

(defmacro with-local-quit (&rest body)
  "Execute BODY, allowing quits to terminate BODY but not escape further.
When a quit terminates BODY, `with-local-quit' returns nil but
requests another quit.  That quit will be processed as soon as quitting
is allowed once again.  (Immediately, if `inhibit-quit' is nil.)"
  (declare (debug t) (indent 0))
  `(condition-case nil
       (let ((inhibit-quit nil))
	 ,@body)
     (quit (setq quit-flag t)
	   ;; This call is to give a chance to handle quit-flag
	   ;; in case inhibit-quit is nil.
	   ;; Without this, it will not be handled until the next function
	   ;; call, and that might allow it to exit thru a condition-case
	   ;; that intends to handle the quit signal next time.
	   (eval '(ignore nil)))))

(defmacro while-no-input (&rest body)
  "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym))
	   (or (input-pending-p)
	       (progn ,@body)))))))

(defmacro condition-case-unless-debug (var bodyform &rest handlers)
  "Like `condition-case' except that it does not catch anything when debugging.
More specifically if `debug-on-error' is set, then it does not catch any signal."
  (declare (debug condition-case) (indent 2))
  (let ((bodysym (make-symbol "body")))
    `(let ((,bodysym (lambda () ,bodyform)))
       (if debug-on-error
           (funcall ,bodysym)
         (condition-case ,var
             (funcall ,bodysym)
           ,@handlers)))))

(define-obsolete-function-alias 'condition-case-no-debug
  'condition-case-unless-debug "24.1")

(defmacro with-demoted-errors (&rest body)
  "Run BODY and demote any errors to simple messages.
If `debug-on-error' is non-nil, run BODY without catching its errors.
This is to be used around code which is not expected to signal an error
but which should be robust in the unexpected case that an error is signaled."
  (declare (debug t) (indent 0))
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         (progn ,@body)
       (error (message "Error: %S" ,err) nil))))

(defmacro combine-after-change-calls (&rest body)
  "Execute BODY, but don't call the after-change functions till the end.
If BODY makes changes in the buffer, they are recorded
and the functions on `after-change-functions' are called several times
when BODY is finished.
The return value is the value of the last form in BODY.

If `before-change-functions' is non-nil, then calls to the after-change
functions can't be deferred, so in that case this macro has no effect.

Do not alter `after-change-functions' or `before-change-functions'
in BODY."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (let ((combine-after-change-calls t))
	 . ,body)
     (combine-after-change-execute)))

(defmacro with-case-table (table &rest body)
  "Execute the forms in BODY with TABLE as the current case table.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((old-case-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-case-table (current-case-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn (set-case-table ,table)
		  ,@body)
	 (with-current-buffer ,old-buffer
	   (set-case-table ,old-case-table))))))

;;; Matching and match data.

(defvar save-match-data-internal)

;; We use save-match-data-internal as the local variable because
;; that works ok in practice (people should not use that variable elsewhere).
;; We used to use an uninterned symbol; the compiler handles that properly
;; now, but it generates slower code.
(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data.
The value returned is the value of the last form in BODY."
  ;; It is better not to use backquote here,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (declare (indent 0) (debug t))
  (list 'let
	'((save-match-data-internal (match-data)))
	(list 'unwind-protect
	      (cons 'progn body)
	      ;; It is safe to free (evaporate) markers immediately here,
	      ;; as Lisp programs should not copy from save-match-data-internal.
	      '(set-match-data save-match-data-internal 'evaporate))))

(defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING.
If STRING is nil, the current buffer should be the same buffer
the search/match was performed in."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defun match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING.
If STRING is nil, the current buffer should be the same buffer
the search/match was performed in."
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))


(defun match-substitute-replacement (replacement
				     &optional fixedcase literal string subexp)
  "Return REPLACEMENT as it will be inserted by `replace-match'.
In other words, all back-references in the form `\\&' and `\\N'
are substituted with actual strings matched by the last search.
Optional FIXEDCASE, LITERAL, STRING and SUBEXP have the same
meaning as for `replace-match'."
  (let ((match (match-string 0 string)))
    (save-match-data
      (set-match-data (mapcar (lambda (x)
				(if (numberp x)
				    (- x (match-beginning 0))
				  x))
			      (match-data t)))
      (replace-match replacement fixedcase literal match subexp))))


(defun looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as
possible, stopping when a single additional previous character
cannot be part of a match for REGEXP.  When the match is
extended, its starting position is allowed to occur before
LIMIT."
  (let ((start (point))
	(pos
	 (save-excursion
	   (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
		(point)))))
    (if (and greedy pos)
	(save-restriction
	  (narrow-to-region (point-min) start)
	  (while (and (> pos (point-min))
		      (save-excursion
			(goto-char pos)
			(backward-char 1)
			(looking-at (concat "\\(?:"  regexp "\\)\\'"))))
	    (setq pos (1- pos)))
	  (save-excursion
	    (goto-char pos)
	    (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
    (not (null pos))))

(defsubst looking-at-p (regexp)
  "\
Same as `looking-at' except this function does not change the match data."
  (let ((inhibit-changing-match-data t))
    (looking-at regexp)))

(defsubst string-match-p (regexp string &optional start)
  "\
Same as `string-match' except this function does not change the match data."
  (let ((inhibit-changing-match-data t))
    (string-match regexp string start)))

(defun subregexp-context-p (regexp pos &optional start)
  "Return non-nil if POS is in a normal subregexp context in REGEXP.
A subregexp context is one where a sub-regexp can appear.
A non-subregexp context is for example within brackets, or within a
repetition bounds operator `\\=\\{...\\}', or right after a `\\'.
If START is non-nil, it should be a position in REGEXP, smaller
than POS, and known to be in a subregexp context."
  ;; Here's one possible implementation, with the great benefit that it
  ;; reuses the regexp-matcher's own parser, so it understands all the
  ;; details of the syntax.  A disadvantage is that it needs to match the
  ;; error string.
  (condition-case err
      (progn
        (string-match (substring regexp (or start 0) pos) "")
        t)
    (invalid-regexp
     (not (member (cadr err) '("Unmatched [ or [^"
                               "Unmatched \\{"
                               "Trailing backslash")))))
  ;; An alternative implementation:
  ;; (defconst re-context-re
  ;;   (let* ((harmless-ch "[^\\[]")
  ;;          (harmless-esc "\\\\[^{]")
  ;;          (class-harmless-ch "[^][]")
  ;;          (class-lb-harmless "[^]:]")
  ;;          (class-lb-colon-maybe-charclass ":\\([a-z]+:]\\)?")
  ;;          (class-lb (concat "\\[\\(" class-lb-harmless
  ;;                            "\\|" class-lb-colon-maybe-charclass "\\)"))
  ;;          (class
  ;;           (concat "\\[^?]?"
  ;;                   "\\(" class-harmless-ch
  ;;                   "\\|" class-lb "\\)*"
  ;;                   "\\[?]"))     ; special handling for bare [ at end of re
  ;;          (braces "\\\\{[0-9,]+\\\\}"))
  ;;     (concat "\\`\\(" harmless-ch "\\|" harmless-esc
  ;;             "\\|" class "\\|" braces "\\)*\\'"))
  ;;   "Matches any prefix that corresponds to a normal subregexp context.")
  ;; (string-match re-context-re (substring regexp (or start 0) pos))
  )

;;;; split-string

(defconst split-string-default-separators "[ \f\t\n\r\v]+"
  "The default value of separators for `split-string'.

A regexp matching strings of whitespace.  May be locale-dependent
\(as yet unimplemented).  Should not match non-breaking spaces.

Warning: binding this to a different value and using it as default is
likely to have undesired semantics.")

;; The specification says that if both SEPARATORS and OMIT-NULLS are
;; defaulted, OMIT-NULLS should be treated as t.  Simplifying the logical
;; expression leads to the equivalent implementation that if SEPARATORS
;; is defaulted, OMIT-NULLS is treated as t.
(defun split-string (string &optional separators omit-nulls)
  "Split STRING into substrings bounded by matches for SEPARATORS.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and
the substrings between the splitting points are collected as a list,
which is returned.

If SEPARATORS is non-nil, it should be a regular expression matching text
which separates, but is not part of, the substrings.  If nil it defaults to
`split-string-default-separators', normally \"[ \\f\\t\\n\\r\\v]+\", and
OMIT-NULLS is forced to t.

If OMIT-NULLS is t, zero-length substrings are omitted from the list \(so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If nil, all zero-length substrings are retained,
which correctly parses CSV format, for example.

Note that the effect of `(split-string STRING)' is the same as
`(split-string STRING split-string-default-separators t)'.  In the rare
case that you wish to retain zero-length substrings when splitting on
whitespace, use `(split-string STRING split-string-default-separators)'.

Modifies the match data; use `save-match-data' if necessary."
  (let ((keep-nulls (not (if separators omit-nulls t)))
	(rexp (or separators split-string-default-separators))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< start (length string)))
      (setq notfirst t)
      (if (or keep-nulls (< start (match-beginning 0)))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (if (or keep-nulls (< start (length string)))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun combine-and-quote-strings (strings &optional separator)
  "Concatenate the STRINGS, adding the SEPARATOR (default \" \").
This tries to quote the strings to avoid ambiguity such that
  (split-string-and-unquote (combine-and-quote-strings strs)) == strs
Only some SEPARATORs will work properly."
  (let* ((sep (or separator " "))
         (re (concat "[\\\"]" "\\|" (regexp-quote sep))))
    (mapconcat
     (lambda (str)
       (if (string-match re str)
	   (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" str) "\"")
	 str))
     strings sep)))

(defun split-string-and-unquote (string &optional separator)
  "Split the STRING into a list of strings.
It understands Emacs Lisp quoting within STRING, such that
  (split-string-and-unquote (combine-and-quote-strings strs)) == strs
The SEPARATOR regexp defaults to \"\\s-+\"."
  (let ((sep (or separator "\\s-+"))
	(i (string-match "\"" string)))
    (if (null i)
	(split-string string sep t)	; no quoting:  easy
      (append (unless (eq i 0) (split-string (substring string 0 i) sep t))
	      (let ((rfs (read-from-string string i)))
		(cons (car rfs)
		      (split-string-and-unquote (substring string (cdr rfs))
						sep)))))))


;;;; Replacement in strings.

(defun subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

(defun replace-regexp-in-string (regexp rep string &optional
					fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function, it is called with the actual text of each
match, and its value is used as the replacement text.  When REP is called,
the match data are the result of matching REGEXP against a substring
of STRING.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacements it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(defun string-prefix-p (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
  (eq t (compare-strings str1 nil nil
                         str2 0 (length str1) ignore-case)))

(defun bidi-string-mark-left-to-right (str)
  "Return a string that can be safely inserted in left-to-right text.

Normally, inserting a string with right-to-left (RTL) script into
a buffer may cause some subsequent text to be displayed as part
of the RTL segment (usually this affects punctuation characters).
This function returns a string which displays as STR but forces
subsequent text to be displayed as left-to-right.

If STR contains any RTL character, this function returns a string
consisting of STR followed by an invisible left-to-right mark
\(LRM) character.  Otherwise, it returns STR."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (if (string-match "\\cR" str)
      (concat str (propertize (string ?\x200e) 'invisible t))
    str))

;;;; invisibility specs

(defun add-to-invisibility-spec (element)
  "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (if (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec (list t)))
  (setq buffer-invisibility-spec
	(cons element buffer-invisibility-spec)))

(defun remove-from-invisibility-spec (element)
  "Remove ELEMENT from `buffer-invisibility-spec'."
  (if (consp buffer-invisibility-spec)
      (setq buffer-invisibility-spec
	    (delete element buffer-invisibility-spec))))

;;;; Syntax tables.

(defmacro with-syntax-table (table &rest body)
  "Evaluate BODY with syntax table of current buffer set to TABLE.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (declare (debug t) (indent 1))
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table ,table)
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))

(defun make-syntax-table (&optional oldtable)
  "Return a new syntax table.
Create a syntax table which inherits from OLDTABLE (if non-nil) or
from `standard-syntax-table' otherwise."
  (let ((table (make-char-table 'syntax-table nil)))
    (set-char-table-parent table (or oldtable (standard-syntax-table)))
    table))

(defun syntax-after (pos)
  "Return the raw syntax of the char after POS.
If POS is outside the buffer's accessible portion, return nil."
  (unless (or (< pos (point-min)) (>= pos (point-max)))
    (let ((st (if parse-sexp-lookup-properties
		  (get-char-property pos 'syntax-table))))
      (if (consp st) st
	(aref (or st (syntax-table)) (char-after pos))))))

(defun syntax-class (syntax)
  "Return the syntax class part of the syntax descriptor SYNTAX.
If SYNTAX is nil, return nil."
  (and syntax (logand (car syntax) 65535)))

;;;; Text clones

(defun text-clone-maintain (ol1 after beg end &optional _len)
  "Propagate the changes made under the overlay OL1 to the other clones.
This is used on the `modification-hooks' property of text clones."
  (when (and after (not undo-in-progress) (overlay-start ol1))
    (let ((margin (if (overlay-get ol1 'text-clone-spreadp) 1 0)))
      (setq beg (max beg (+ (overlay-start ol1) margin)))
      (setq end (min end (- (overlay-end ol1) margin)))
      (when (<= beg end)
	(save-excursion
	  (when (overlay-get ol1 'text-clone-syntax)
	    ;; Check content of the clone's text.
	    (let ((cbeg (+ (overlay-start ol1) margin))
		  (cend (- (overlay-end ol1) margin)))
	      (goto-char cbeg)
	      (save-match-data
		(if (not (re-search-forward
			  (overlay-get ol1 'text-clone-syntax) cend t))
		    ;; Mark the overlay for deletion.
		    (overlay-put ol1 'text-clones nil)
		  (when (< (match-end 0) cend)
		    ;; Shrink the clone at its end.
		    (setq end (min end (match-end 0)))
		    (move-overlay ol1 (overlay-start ol1)
				  (+ (match-end 0) margin)))
		  (when (> (match-beginning 0) cbeg)
		    ;; Shrink the clone at its beginning.
		    (setq beg (max (match-beginning 0) beg))
		    (move-overlay ol1 (- (match-beginning 0) margin)
				  (overlay-end ol1)))))))
	  ;; Now go ahead and update the clones.
	  (let ((head (- beg (overlay-start ol1)))
		(tail (- (overlay-end ol1) end))
		(str (buffer-substring beg end))
		(nothing-left t)
		(inhibit-modification-hooks t))
	    (dolist (ol2 (overlay-get ol1 'text-clones))
	      (let ((oe (overlay-end ol2)))
		(unless (or (eq ol1 ol2) (null oe))
		  (setq nothing-left nil)
		  (let ((mod-beg (+ (overlay-start ol2) head)))
		    ;;(overlay-put ol2 'modification-hooks nil)
		    (goto-char (- (overlay-end ol2) tail))
		    (unless (> mod-beg (point))
		      (save-excursion (insert str))
		      (delete-region mod-beg (point)))
		    ;;(overlay-put ol2 'modification-hooks '(text-clone-maintain))
		    ))))
	    (if nothing-left (delete-overlay ol1))))))))

(defun text-clone-create (start end &optional spreadp syntax)
  "Create a text clone of START...END at point.
Text clones are chunks of text that are automatically kept identical:
changes done to one of the clones will be immediately propagated to the other.

The buffer's content at point is assumed to be already identical to
the one between START and END.
If SYNTAX is provided it's a regexp that describes the possible text of
the clones; the clone will be shrunk or killed if necessary to ensure that
its text matches the regexp.
If SPREADP is non-nil it indicates that text inserted before/after the
clone should be incorporated in the clone."
  ;; To deal with SPREADP we can either use an overlay with `nil t' along
  ;; with insert-(behind|in-front-of)-hooks or use a slightly larger overlay
  ;; (with a one-char margin at each end) with `t nil'.
  ;; We opted for a larger overlay because it behaves better in the case
  ;; where the clone is reduced to the empty string (we want the overlay to
  ;; stay when the clone's content is the empty string and we want to use
  ;; `evaporate' to make sure those overlays get deleted when needed).
  ;;
  (let* ((pt-end (+ (point) (- end start)))
  	 (start-margin (if (or (not spreadp) (bobp) (<= start (point-min)))
			   0 1))
  	 (end-margin (if (or (not spreadp)
			     (>= pt-end (point-max))
  			     (>= start (point-max)))
  			 0 1))
  	 (ol1 (make-overlay (- start start-margin) (+ end end-margin) nil t))
  	 (ol2 (make-overlay (- (point) start-margin) (+ pt-end end-margin) nil t))
	 (dups (list ol1 ol2)))
    (overlay-put ol1 'modification-hooks '(text-clone-maintain))
    (when spreadp (overlay-put ol1 'text-clone-spreadp t))
    (when syntax (overlay-put ol1 'text-clone-syntax syntax))
    ;;(overlay-put ol1 'face 'underline)
    (overlay-put ol1 'evaporate t)
    (overlay-put ol1 'text-clones dups)
    ;;
    (overlay-put ol2 'modification-hooks '(text-clone-maintain))
    (when spreadp (overlay-put ol2 'text-clone-spreadp t))
    (when syntax (overlay-put ol2 'text-clone-syntax syntax))
    ;;(overlay-put ol2 'face 'underline)
    (overlay-put ol2 'evaporate t)
    (overlay-put ol2 'text-clones dups)))

;;;; Mail user agents.

;; Here we include just enough for other packages to be able
;; to define them.

(defun define-mail-user-agent (symbol composefunc sendfunc
				      &optional abortfunc hookvar)
  "Define a symbol to identify a mail-sending package for `mail-user-agent'.

SYMBOL can be any Lisp symbol.  Its function definition and/or
value as a variable do not matter for this usage; we use only certain
properties on its property list, to encode the rest of the arguments.

COMPOSEFUNC is program callable function that composes an outgoing
mail message buffer.  This function should set up the basics of the
buffer without requiring user interaction.  It should populate the
standard mail headers, leaving the `to:' and `subject:' headers blank
by default.

COMPOSEFUNC should accept several optional arguments--the same
arguments that `compose-mail' takes.  See that function's documentation.

SENDFUNC is the command a user would run to send the message.

Optional ABORTFUNC is the command a user would run to abort the
message.  For mail packages that don't have a separate abort function,
this can be `kill-buffer' (the equivalent of omitting this argument).

Optional HOOKVAR is a hook variable that gets run before the message
is actually sent.  Callers that use the `mail-user-agent' may
install a hook function temporarily on this hook variable.
If HOOKVAR is nil, `mail-send-hook' is used.

The properties used on SYMBOL are `composefunc', `sendfunc',
`abortfunc', and `hookvar'."
  (put symbol 'composefunc composefunc)
  (put symbol 'sendfunc sendfunc)
  (put symbol 'abortfunc (or abortfunc 'kill-buffer))
  (put symbol 'hookvar (or hookvar 'mail-send-hook)))

;;;; Progress reporters.

;; Progress reporter has the following structure:
;;
;;	(NEXT-UPDATE-VALUE . [NEXT-UPDATE-TIME
;;			      MIN-VALUE
;;			      MAX-VALUE
;;			      MESSAGE
;;			      MIN-CHANGE
;;			      MIN-TIME])
;;
;; This weirdness is for optimization reasons: we want
;; `progress-reporter-update' to be as fast as possible, so
;; `(car reporter)' is better than `(aref reporter 0)'.
;;
;; NEXT-UPDATE-TIME is a float.  While `float-time' loses a couple
;; digits of precision, it doesn't really matter here.  On the other
;; hand, it greatly simplifies the code.

(defsubst progress-reporter-update (reporter &optional value)
  "Report progress of an operation in the echo area.
REPORTER should be the result of a call to `make-progress-reporter'.

If REPORTER is a numerical progress reporter---i.e. if it was
 made using non-nil MIN-VALUE and MAX-VALUE arguments to
 `make-progress-reporter'---then VALUE should be a number between
 MIN-VALUE and MAX-VALUE.

If REPORTER is a non-numerical reporter, VALUE should be nil.

This function is relatively inexpensive.  If the change since
last update is too small or insufficient time has passed, it does
nothing."
  (when (or (not (numberp value))      ; For pulsing reporter
	    (>= value (car reporter))) ; For numerical reporter
    (progress-reporter-do-update reporter value)))

(defun make-progress-reporter (message &optional min-value max-value
				       current-value min-change min-time)
  "Return progress reporter object for use with `progress-reporter-update'.

MESSAGE is shown in the echo area, with a status indicator
appended to the end.  When you call `progress-reporter-done', the
word \"done\" is printed after the MESSAGE.  You can change the
MESSAGE of an existing progress reporter by calling
`progress-reporter-force-update'.

MIN-VALUE and MAX-VALUE, if non-nil, are starting (0% complete)
and final (100% complete) states of operation; the latter should
be larger.  In this case, the status message shows the percentage
progress.

If MIN-VALUE and/or MAX-VALUE is omitted or nil, the status
message shows a \"spinning\", non-numeric indicator.

Optional CURRENT-VALUE is the initial progress; the default is
MIN-VALUE.
Optional MIN-CHANGE is the minimal change in percents to report;
the default is 1%.
CURRENT-VALUE and MIN-CHANGE do not have any effect if MIN-VALUE
and/or MAX-VALUE are nil.

Optional MIN-TIME specifies the minimum interval time between
echo area updates (default is 0.2 seconds.)  If the function
`float-time' is not present, time is not tracked at all.  If the
OS is not capable of measuring fractions of seconds, this
parameter is effectively rounded up."
  (when (string-match "[[:alnum:]]\\'" message)
    (setq message (concat message "...")))
  (unless min-time
    (setq min-time 0.2))
  (let ((reporter
	 ;; Force a call to `message' now
	 (cons (or min-value 0)
	       (vector (if (and (fboundp 'float-time)
				(>= min-time 0.02))
			   (float-time) nil)
		       min-value
		       max-value
		       message
		       (if min-change (max (min min-change 50) 1) 1)
		       min-time))))
    (progress-reporter-update reporter (or current-value min-value))
    reporter))

(defun progress-reporter-force-update (reporter &optional value new-message)
  "Report progress of an operation in the echo area unconditionally.

The first two arguments are the same as in `progress-reporter-update'.
NEW-MESSAGE, if non-nil, sets a new message for the reporter."
  (let ((parameters (cdr reporter)))
    (when new-message
      (aset parameters 3 new-message))
    (when (aref parameters 0)
      (aset parameters 0 (float-time)))
    (progress-reporter-do-update reporter value)))

(defvar progress-reporter--pulse-characters ["-" "\\" "|" "/"]
  "Characters to use for pulsing progress reporters.")

(defun progress-reporter-do-update (reporter value)
  (let* ((parameters   (cdr reporter))
	 (update-time  (aref parameters 0))
	 (min-value    (aref parameters 1))
	 (max-value    (aref parameters 2))
	 (text         (aref parameters 3))
	 (current-time (float-time))
	 (enough-time-passed
	  ;; See if enough time has passed since the last update.
	  (or (not update-time)
	      (when (>= current-time update-time)
		;; Calculate time for the next update
		(aset parameters 0 (+ update-time (aref parameters 5)))))))
    (cond ((and min-value max-value)
	   ;; Numerical indicator
	   (let* ((one-percent (/ (- max-value min-value) 100.0))
		  (percentage  (if (= max-value min-value)
				   0
				 (truncate (/ (- value min-value)
					      one-percent)))))
	     ;; Calculate NEXT-UPDATE-VALUE.  If we are not printing
	     ;; message because not enough time has passed, use 1
	     ;; instead of MIN-CHANGE.  This makes delays between echo
	     ;; area updates closer to MIN-TIME.
	     (setcar reporter
		     (min (+ min-value (* (+ percentage
					     (if enough-time-passed
						 ;; MIN-CHANGE
						 (aref parameters 4)
					       1))
					  one-percent))
			  max-value))
	     (when (integerp value)
	       (setcar reporter (ceiling (car reporter))))
	     ;; Only print message if enough time has passed
	     (when enough-time-passed
	       (if (> percentage 0)
		   (message "%s%d%%" text percentage)
		 (message "%s" text)))))
	  ;; Pulsing indicator
	  (enough-time-passed
	   (let ((index (mod (1+ (car reporter)) 4))
		 (message-log-max nil))
	     (setcar reporter index)
	     (message "%s %s"
		      text
		      (aref progress-reporter--pulse-characters
			    index)))))))

(defun progress-reporter-done (reporter)
  "Print reporter's message followed by word \"done\" in echo area."
  (message "%sdone" (aref (cdr reporter) 3)))

(defmacro dotimes-with-progress-reporter (spec message &rest body)
  "Loop a certain number of times and report progress in the echo area.
Evaluate BODY with VAR bound to successive integers running from
0, inclusive, to COUNT, exclusive.  Then evaluate RESULT to get
the return value (nil if RESULT is omitted).

At each iteration MESSAGE followed by progress percentage is
printed in the echo area.  After the loop is finished, MESSAGE
followed by word \"done\" is printed.  This macro is a
convenience wrapper around `make-progress-reporter' and friends.

\(fn (VAR COUNT [RESULT]) MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (let ((temp (make-symbol "--dotimes-temp--"))
	(temp2 (make-symbol "--dotimes-temp2--"))
	(start 0)
	(end (nth 1 spec)))
    `(let ((,temp ,end)
	   (,(car spec) ,start)
	   (,temp2 (make-progress-reporter ,message ,start ,end)))
       (while (< ,(car spec) ,temp)
	 ,@body
	 (progress-reporter-update ,temp2
				   (setq ,(car spec) (1+ ,(car spec)))))
       (progress-reporter-done ,temp2)
       nil ,@(cdr (cdr spec)))))


;;;; Comparing version strings.

(defconst version-separator "."
  "Specify the string used to separate the version elements.

Usually the separator is \".\", but it can be any other string.")


(defconst version-regexp-alist
  '(("^[-_+ ]?alpha$"           . -3)
    ("^[-_+]$"                  . -3) ; treat "1.2.3-20050920" and "1.2-3" as alpha releases
    ("^[-_+ ]cvs$"              . -3) ; treat "1.2.3-CVS" as alpha release
    ("^[-_+ ]?beta$"            . -2)
    ("^[-_+ ]?\\(pre\\|rcc\\)$" . -1))
  "Specify association between non-numeric version and its priority.

This association is used to handle version string like \"1.0pre2\",
\"0.9alpha1\", etc.  It's used by `version-to-list' (which see) to convert the
non-numeric part of a version string to an integer.  For example:

   String Version    Integer List Version
   \"1.0pre2\"         (1  0 -1 2)
   \"1.0PRE2\"         (1  0 -1 2)
   \"22.8beta3\"       (22 8 -2 3)
   \"22.8 Beta3\"      (22 8 -2 3)
   \"0.9alpha1\"       (0  9 -3 1)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9 alpha\"       (0  9 -3)

Each element has the following form:

   (REGEXP . PRIORITY)

Where:

REGEXP		regexp used to match non-numeric part of a version string.
		It should begin with the `^' anchor and end with a `$' to
		prevent false hits.  Letter-case is ignored while matching
		REGEXP.

PRIORITY	a negative integer specifying non-numeric priority of REGEXP.")


(defun version-to-list (ver)
  "Convert version string VER into a list of integers.

The version syntax is given by the following EBNF:

   VERSION ::= NUMBER ( SEPARATOR NUMBER )*.

   NUMBER ::= (0|1|2|3|4|5|6|7|8|9)+.

   SEPARATOR ::= `version-separator' (which see)
	       | `version-regexp-alist' (which see).

The NUMBER part is optional if SEPARATOR is a match for an element
in `version-regexp-alist'.

Examples of valid version syntax:

   1.0pre2   1.0.7.5   22.8beta3   0.9alpha1   6.9.30Beta

Examples of invalid version syntax:

   1.0prepre2   1.0..7.5   22.8X3   alpha3.2   .5

Examples of version conversion:

   Version String    Version as a List of Integers
   \"1.0.7.5\"         (1  0  7 5)
   \"1.0pre2\"         (1  0 -1 2)
   \"1.0PRE2\"         (1  0 -1 2)
   \"22.8beta3\"       (22 8 -2 3)
   \"22.8Beta3\"       (22 8 -2 3)
   \"0.9alpha1\"       (0  9 -3 1)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9alpha\"        (0  9 -3)

See documentation for `version-separator' and `version-regexp-alist'."
  (or (and (stringp ver) (> (length ver) 0))
      (error "Invalid version string: '%s'" ver))
  ;; Change .x.y to 0.x.y
  (if (and (>= (length ver) (length version-separator))
	   (string-equal (substring ver 0 (length version-separator))
			 version-separator))
      (setq ver (concat "0" ver)))
  (save-match-data
    (let ((i 0)
	  (case-fold-search t)		; ignore case in matching
	  lst s al)
      (while (and (setq s (string-match "[0-9]+" ver i))
		  (= s i))
	;; handle numeric part
	(setq lst (cons (string-to-number (substring ver i (match-end 0)))
			lst)
	      i   (match-end 0))
	;; handle non-numeric part
	(when (and (setq s (string-match "[^0-9]+" ver i))
		   (= s i))
	  (setq s (substring ver i (match-end 0))
		i (match-end 0))
	  ;; handle alpha, beta, pre, etc. separator
	  (unless (string= s version-separator)
	    (setq al version-regexp-alist)
	    (while (and al (not (string-match (caar al) s)))
	      (setq al (cdr al)))
	    (cond (al
		   (push (cdar al) lst))
		  ;; Convert 22.3a to 22.3.1, 22.3b to 22.3.2, etc.
		  ((string-match "^[-_+ ]?\\([a-zA-Z]\\)$" s)
		   (push (- (aref (downcase (match-string 1 s)) 0) ?a -1)
			 lst))
		  (t (error "Invalid version syntax: '%s'" ver))))))
      (if (null lst)
	  (error "Invalid version syntax: '%s'" ver)
	(nreverse lst)))))


(defun version-list-< (l1 l2)
  "Return t if L1, a list specification of a version, is lower than L2.

Note that a version specified by the list (1) is equal to (1 0),
\(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
Also, a version given by the list (1) is higher than (1 -1), which in
turn is higher than (1 -2), which is higher than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null         ==> l1 length = l2 length
   ((and (null l1) (null l2)) nil)
   ;; l1 not null and l2 null     ==> l1 length > l2 length
   (l1 (< (version-list-not-zero l1) 0))
   ;; l1 null and l2 not null     ==> l2 length > l1 length
   (t  (< 0 (version-list-not-zero l2)))))


(defun version-list-= (l1 l2)
  "Return t if L1, a list specification of a version, is equal to L2.

Note that a version specified by the list (1) is equal to (1 0),
\(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
Also, a version given by the list (1) is higher than (1 -1), which in
turn is higher than (1 -2), which is higher than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) nil)
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (zerop (version-list-not-zero l1)))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (zerop (version-list-not-zero l2)))))


(defun version-list-<= (l1 l2)
  "Return t if L1, a list specification of a version, is lower or equal to L2.

Note that integer list (1) is equal to (1 0), (1 0 0), (1 0 0 0),
etc.  That is, the trailing zeroes are insignificant.  Also, integer
list (1) is greater than (1 -1) which is greater than (1 -2)
which is greater than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (<= (version-list-not-zero l1) 0))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (<= 0 (version-list-not-zero l2)))))

(defun version-list-not-zero (lst)
  "Return the first non-zero element of LST, which is a list of integers.

If all LST elements are zeros or LST is nil, return zero."
  (while (and lst (zerop (car lst)))
    (setq lst (cdr lst)))
  (if lst
      (car lst)
    ;; there is no element different of zero
    0))


(defun version< (v1 v2)
  "Return t if version V1 is lower (older) than V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\".  Also, \"-CVS\" and \"-NNN\" are treated
as alpha versions."
  (version-list-< (version-to-list v1) (version-to-list v2)))


(defun version<= (v1 v2)
  "Return t if version V1 is lower (older) than or equal to V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\".  Also, \"-CVS\" and \"-NNN\" are treated
as alpha versions."
  (version-list-<= (version-to-list v1) (version-to-list v2)))

(defun version= (v1 v2)
  "Return t if version V1 is equal to V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\".  Also, \"-CVS\" and \"-NNN\" are treated
as alpha versions."
  (version-list-= (version-to-list v1) (version-to-list v2)))


;;; Misc.
(defconst menu-bar-separator '("--")
  "Separator for menus.")

;; The following statement ought to be in print.c, but `provide' can't
;; be used there.
;; http://lists.gnu.org/archive/html/emacs-devel/2009-08/msg00236.html
(when (hash-table-p (car (read-from-string
			  (prin1-to-string (make-hash-table)))))
  (provide 'hashtable-print-readable))

;;; subr.el ends here
