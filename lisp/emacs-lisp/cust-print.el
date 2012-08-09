;;; cust-print.el --- handles print-level and print-circle

;; Copyright (C) 1992, 2001-2012  Free Software Foundation, Inc.

;; Author: Daniel LaLiberte <liberte@holonexus.org>
;; Adapted-By: ESR
;; Keywords: extensions

;; LCD Archive Entry:
;; cust-print|Daniel LaLiberte|liberte@holonexus.org
;; |Handle print-level, print-circle and more.

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

;; This package provides a general print handler for prin1 and princ
;; that supports print-level and print-circle, and by the way,
;; print-length since the standard routines are being replaced.  Also,
;; to print custom types constructed from lists and vectors, use
;; custom-print-list and custom-print-vector.  See the documentation
;; strings of these variables for more details.

;; If the results of your expressions contain circular references to
;; other parts of the same structure, the standard Emacs print
;; subroutines may fail to print with an untrappable error,
;; "Apparently circular structure being printed".  If you only use cdr
;; circular lists (where cdrs of lists point back; what is the right
;; term here?), you can limit the length of printing with
;; print-length.  But car circular lists and circular vectors generate
;; the above mentioned error in Emacs version 18.  Version
;; 19 supports print-level, but it is often useful to get a better
;; print representation of circular and shared structures; the print-circle
;; option may be used to print more concise representations.

;; There are three main ways to use this package.  First, you may
;; replace prin1, princ, and some subroutines that use them by calling
;; install-custom-print so that any use of these functions in
;; Lisp code will be affected; you can later reset with
;; uninstall-custom-print.  Second, you may temporarily install
;; these functions with the macro with-custom-print.  Third, you
;; could call the custom routines directly, thus only affecting the
;; printing that requires them.

;; Note that subroutines which call print subroutines directly will
;; not use the custom print functions.  In particular, the evaluation
;; functions like eval-region call the print subroutines directly.
;; Therefore, if you evaluate (aref circ-list 0), where circ-list is a
;; circular list rather than an array, aref calls error directly which
;; will jump to the top level instead of printing the circular list.

;; Uninterned symbols are recognized when print-circle is non-nil,
;; but they are not printed specially here.  Use the cl-packages package
;; to print according to print-gensym.

;; Obviously the right way to implement this custom-print facility is
;; in C or with hooks into the standard printer.  Please volunteer
;; since I don't have the time or need.  More CL-like printing
;; capabilities could be added in the future.

;; Implementation design: we want to use the same list and vector
;; processing algorithm for all versions of prin1 and princ, since how
;; the processing is done depends on print-length, print-level, and
;; print-circle.  For circle printing, a preprocessing step is
;; required before the final printing.  Thanks to Jamie Zawinski
;; for motivation and algorithms.


;;; Code:

(defgroup cust-print nil
  "Handles print-level and print-circle."
  :prefix "print-"
  :group 'lisp
  :group 'extensions)

;; If using cl-packages:

'(defpackage "cust-print"
   (:nicknames "CP" "custom-print")
   (:use "el")
   (:export
    print-level
    print-circle

    custom-print-install
    custom-print-uninstall
    custom-print-installed-p
    with-custom-print

    custom-prin1
    custom-princ
    custom-prin1-to-string
    custom-print
    custom-format
    custom-message
    custom-error

    custom-printers
    add-custom-printer
    ))

'(in-package cust-print)

;; Emacs 18 doesn't have defalias.
;; Provide def for byte compiler.
(eval-and-compile
  (or (fboundp 'defalias) (fset 'defalias 'fset)))


;; Variables:
;;=========================================================

;;(defvar print-length nil
;;  "*Controls how many elements of a list, at each level, are printed.
;;This is defined by emacs.")

(defcustom print-level nil
  "Controls how many levels deep a nested data object will print.

If nil, printing proceeds recursively and may lead to
max-lisp-eval-depth being exceeded or an error may occur:
`Apparently circular structure being printed.'
Also see `print-length' and `print-circle'.

If non-nil, components at levels equal to or greater than `print-level'
are printed simply as `#'.  The object to be printed is at level 0,
and if the object is a list or vector, its top-level components are at
level 1."
  :type '(choice (const nil) integer)
  :group 'cust-print)


(defcustom print-circle nil
  "Controls the printing of recursive structures.

If nil, printing proceeds recursively and may lead to
`max-lisp-eval-depth' being exceeded or an error may occur:
\"Apparently circular structure being printed.\"  Also see
`print-length' and `print-level'.

If non-nil, shared substructures anywhere in the structure are printed
with `#N=' before the first occurrence (in the order of the print
representation) and `#N#' in place of each subsequent occurrence,
where N is a positive decimal integer.

There is no way to read this representation in standard Emacs,
but if you need to do so, try the cl-read.el package."
  :type 'boolean
  :group 'cust-print)


(defcustom custom-print-vectors nil
  "Non-nil if printing of vectors should obey `print-level' and `print-length'."
  :type 'boolean
  :group 'cust-print)


;; Custom printers
;;==========================================================

(defvar custom-printers nil
  ;; e.g. '((symbolp . pkg::print-symbol))
  "An alist for custom printing of any type.
Pairs are of the form (PREDICATE . PRINTER).  If PREDICATE is true
for an object, then PRINTER is called with the object.
PRINTER should print to `standard-output' using cust-print-original-princ
if the standard printer is sufficient, or cust-print-prin for complex things.
The PRINTER should return the object being printed.

Don't modify this variable directly.  Use `add-custom-printer' and
`delete-custom-printer'")
;; Should cust-print-original-princ and cust-print-prin be exported symbols?
;; Or should the standard printers functions be replaced by
;; CP ones in Emacs Lisp so that CP internal functions need not be called?

(defun add-custom-printer (pred printer)
  "Add a pair of PREDICATE and PRINTER to `custom-printers'.
Any pair that has the same PREDICATE is first removed."
  (setq custom-printers (cons (cons pred printer)
			      (delq (assq pred custom-printers)
				    custom-printers)))
  ;; Rather than updating here, we could wait until cust-print-top-level is called.
  (cust-print-update-custom-printers))

(defun delete-custom-printer (pred)
  "Delete the custom printer associated with PREDICATE."
  (setq custom-printers (delq (assq pred custom-printers)
			      custom-printers))
  (cust-print-update-custom-printers))


(defun cust-print-use-custom-printer (object)
  ;; Default function returns nil.
  nil)

(defun cust-print-update-custom-printers ()
  ;; Modify the definition of cust-print-use-custom-printer
  (defalias 'cust-print-use-custom-printer
    ;; We don't really want to require the byte-compiler.
    ;; (byte-compile
    `(lambda (object)
       (cond
	,@(mapcar (function
		   (lambda (pair)
		     `((,(car pair) object)
		       (,(cdr pair) object))))
		  custom-printers)
	;; Otherwise return nil.
	(t nil)
	))
    ;; )
    ))


;; Saving and restoring emacs printing routines.
;;====================================================

(defun cust-print-set-function-cell (symbol-pair)
  (defalias (car symbol-pair)
    (symbol-function (car (cdr symbol-pair)))))

(defun cust-print-original-princ (object &optional stream)) ; dummy def

;; Save emacs routines.
(if (not (fboundp 'cust-print-original-prin1))
    (mapc 'cust-print-set-function-cell
	  '((cust-print-original-prin1 prin1)
	    (cust-print-original-princ princ)
	    (cust-print-original-print print)
	    (cust-print-original-prin1-to-string prin1-to-string)
	    (cust-print-original-format format)
	    (cust-print-original-message message)
	    (cust-print-original-error error))))


(defun custom-print-install ()
  "Replace print functions with general, customizable, Lisp versions.
The Emacs subroutines are saved away, and you can reinstall them
by running `custom-print-uninstall'."
  (interactive)
  (mapc 'cust-print-set-function-cell
	'((prin1 custom-prin1)
	  (princ custom-princ)
	  (print custom-print)
	  (prin1-to-string custom-prin1-to-string)
	  (format custom-format)
	  (message custom-message)
	  (error custom-error)
	  ))
  t)

(defun custom-print-uninstall ()
  "Reset print functions to their Emacs subroutines."
  (interactive)
  (mapc 'cust-print-set-function-cell
	'((prin1 cust-print-original-prin1)
	  (princ cust-print-original-princ)
	  (print cust-print-original-print)
	  (prin1-to-string cust-print-original-prin1-to-string)
	  (format cust-print-original-format)
	  (message cust-print-original-message)
	  (error cust-print-original-error)
	  ))
  t)

(defalias 'custom-print-funcs-installed-p 'custom-print-installed-p)
(defun custom-print-installed-p ()
  "Return t if custom-print is currently installed, nil otherwise."
  (eq (symbol-function 'custom-prin1) (symbol-function 'prin1)))

(put 'with-custom-print-funcs 'edebug-form-spec '(body))
(put 'with-custom-print 'edebug-form-spec '(body))

(defalias 'with-custom-print-funcs 'with-custom-print)
(defmacro with-custom-print (&rest body)
  "Temporarily install the custom print package while executing BODY."
  `(unwind-protect
       (progn
	 (custom-print-install)
	 ,@body)
     (custom-print-uninstall)))


;; Lisp replacements for prin1 and princ, and for some subrs that use them
;;===============================================================
;; - so far only the printing and formatting subrs.

(defun custom-prin1 (object &optional stream)
  "Output the printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).

This is the custom-print replacement for the standard `prin1'.  It
uses the appropriate printer depending on the values of `print-level'
and `print-circle' (which see)."
  (cust-print-top-level object stream 'cust-print-original-prin1))


(defun custom-princ (object &optional stream)
  "Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.
Output stream is STREAM, or value of `standard-output' (which see).

This is the custom-print replacement for the standard `princ'."
  (cust-print-top-level object stream 'cust-print-original-princ))


(defun custom-prin1-to-string (object &optional noescape)
  "Return a string containing the printed representation of OBJECT,
any Lisp object.  Quoting characters are used when needed to make output
that `read' can handle, whenever this is possible, unless the optional
second argument NOESCAPE is non-nil.

This is the custom-print replacement for the standard `prin1-to-string'."
  (let ((buf (get-buffer-create " *custom-print-temp*")))
    ;; We must erase the buffer before printing in case an error
    ;; occurred during the last prin1-to-string and we are in debugger.
    (with-current-buffer buf
      (erase-buffer))
    ;; We must be in the current-buffer when the print occurs.
    (if noescape
	(custom-princ object buf)
      (custom-prin1 object buf))
    (with-current-buffer buf
      (buffer-string)
      ;; We could erase the buffer again, but why bother?
      )))


(defun custom-print (object &optional stream)
  "Output the printed representation of OBJECT, with newlines around it.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).

This is the custom-print replacement for the standard `print'."
  (cust-print-original-princ "\n" stream)
  (custom-prin1 object stream)
  (cust-print-original-princ "\n" stream))


(defun custom-format (fmt &rest args)
  "Format a string out of a control-string and arguments.
The first argument is a control string.  It, and subsequent arguments
substituted into it, become the value, which is a string.
It may contain %s or %d or %c to substitute successive following arguments.
%s means print an argument as a string, %d means print as number in decimal,
%c means print a number as a single character.
The argument used by %s must be a string or a symbol;
the argument used by %d, %b, %o, %x or %c must be a number.

This is the custom-print replacement for the standard `format'.  It
calls the Emacs `format' after first making strings for list,
vector, or symbol args.  The format specification for such args should
be `%s' in any case, so a string argument will also work.  The string
is generated with `custom-prin1-to-string', which quotes quotable
characters."
  (apply 'cust-print-original-format fmt
	 (mapcar (function (lambda (arg)
			     (if (or (listp arg) (vectorp arg) (symbolp arg))
				 (custom-prin1-to-string arg)
			       arg)))
		 args)))


(defun custom-message (fmt &rest args)
  "Print a one-line message at the bottom of the screen.
The first argument is a control string.
It may contain %s or %d or %c to print successive following arguments.
%s means print an argument as a string, %d means print as number in decimal,
%c means print a number as a single character.
The argument used by %s must be a string or a symbol;
the argument used by %d or %c must be a number.

This is the custom-print replacement for the standard `message'.
See `custom-format' for the details."
  ;; It doesn't work to princ the result of custom-format as in:
  ;; (cust-print-original-princ (apply 'custom-format fmt args))
  ;; because the echo area requires special handling
  ;; to avoid duplicating the output.
  ;; cust-print-original-message does it right.
  (apply 'cust-print-original-message  fmt
	 (mapcar (function (lambda (arg)
			     (if (or (listp arg) (vectorp arg) (symbolp arg))
				 (custom-prin1-to-string arg)
			       arg)))
		 args)))


(defun custom-error (fmt &rest args)
  "Signal an error, making error message by passing all args to `format'.

This is the custom-print replacement for the standard `error'.
See `custom-format' for the details."
  (signal 'error (list (apply 'custom-format fmt args))))



;; Support for custom prin1 and princ
;;=========================================

;; Defs to quiet byte-compiler.
(defvar circle-table)
(defvar cust-print-current-level)

(defun cust-print-original-printer (object))  ; One of the standard printers.
(defun cust-print-low-level-prin (object))    ; Used internally.
(defun cust-print-prin (object))              ; Call this to print recursively.

(defun cust-print-top-level (object stream emacs-printer)
  ;; Set up for printing.
  (let ((standard-output (or stream standard-output))
	;; circle-table will be non-nil if anything is circular.
	(circle-table (and print-circle
			   (cust-print-preprocess-circle-tree object)))
	(cust-print-current-level (or print-level -1)))

    (defalias 'cust-print-original-printer emacs-printer)
    (defalias 'cust-print-low-level-prin
      (cond
       ((or custom-printers
	    circle-table
	    print-level			; comment out for version 19
	    ;; Emacs doesn't use print-level or print-length
	    ;; for vectors, but custom-print can.
	    (if custom-print-vectors
		(or print-level print-length)))
	'cust-print-print-object)
       (t 'cust-print-original-printer)))
    (defalias 'cust-print-prin
      (if circle-table 'cust-print-print-circular 'cust-print-low-level-prin))

    (cust-print-prin object)
    object))


(defun cust-print-print-object (object)
  ;; Test object type and print accordingly.
  ;; Could be called as either cust-print-low-level-prin or cust-print-prin.
  (cond
   ((null object) (cust-print-original-printer object))
   ((cust-print-use-custom-printer object) object)
   ((consp object) (cust-print-list object))
   ((vectorp object) (cust-print-vector object))
   ;; All other types, just print.
   (t (cust-print-original-printer object))))


(defun cust-print-print-circular (object)
  ;; Printer for `prin1' and `princ' that handles circular structures.
  ;; If OBJECT appears multiply, and has not yet been printed,
  ;; prefix with label; if it has been printed, use `#N#' instead.
  ;; Otherwise, print normally.
  (let ((tag (assq object circle-table)))
    (if tag
	(let ((id (cdr tag)))
	  (if (> id 0)
	      (progn
		;; Already printed, so just print id.
		(cust-print-original-princ "#")
		(cust-print-original-princ id)
		(cust-print-original-princ "#"))
	    ;; Not printed yet, so label with id and print object.
	    (setcdr tag (- id)) ; mark it as printed
	    (cust-print-original-princ "#")
	    (cust-print-original-princ (- id))
	    (cust-print-original-princ "=")
	    (cust-print-low-level-prin object)
	    ))
      ;; Not repeated in structure.
      (cust-print-low-level-prin object))))


;;================================================
;; List and vector processing for print functions.

(defun cust-print-list (list)
  ;; Print a list using print-length, print-level, and print-circle.
  (if (= cust-print-current-level 0)
      (cust-print-original-princ "#")
    (let ((cust-print-current-level (1- cust-print-current-level)))
      (cust-print-original-princ "(")
      (let ((length (or print-length 0)))

	;; Print the first element always (even if length = 0).
	(cust-print-prin (car list))
	(setq list (cdr list))
	(if list (cust-print-original-princ " "))
	(setq length (1- length))

	;; Print the rest of the elements.
	(while (and list (/= 0 length))
	  (if (and (listp list)
		   (not (assq list circle-table)))
	      (progn
		(cust-print-prin (car list))
		(setq list (cdr list)))

	    ;; cdr is not a list, or it is in circle-table.
	    (cust-print-original-princ ". ")
	    (cust-print-prin list)
	    (setq list nil))

	  (setq length (1- length))
	  (if list (cust-print-original-princ " ")))

	(if (and list (= length 0)) (cust-print-original-princ "..."))
	(cust-print-original-princ ")"))))
  list)


(defun cust-print-vector (vector)
  ;; Print a vector according to print-length, print-level, and print-circle.
  (if (= cust-print-current-level 0)
      (cust-print-original-princ "#")
    (let ((cust-print-current-level (1- cust-print-current-level))
	  (i 0)
	  (len (length vector)))
      (cust-print-original-princ "[")

      (if print-length
	  (setq len (min print-length len)))
      ;; Print the elements
      (while (< i len)
	(cust-print-prin (aref vector i))
	(setq i (1+ i))
	(if (< i (length vector)) (cust-print-original-princ " ")))

      (if (< i (length vector)) (cust-print-original-princ "..."))
      (cust-print-original-princ "]")
      ))
  vector)



;; Circular structure preprocessing
;;==================================

(defun cust-print-preprocess-circle-tree (object)
  ;; Fill up the table.
  (let (;; Table of tags for each object in an object to be printed.
	;; A tag is of the form:
	;; ( <object> <nil-t-or-id-number> )
	;; The id-number is generated after the entire table has been computed.
	;; During walk through, the real circle-table lives in the cdr so we
	;; can use setcdr to add new elements instead of having to setq the
	;; variable sometimes (poor man's locf).
	(circle-table (list nil)))
    (cust-print-walk-circle-tree object)

    ;; Reverse table so it is in the order that the objects will be printed.
    ;; This pass could be avoided if we always added to the end of the
    ;; table with setcdr in walk-circle-tree.
    (setcdr circle-table (nreverse (cdr circle-table)))

    ;; Walk through the table, assigning id-numbers to those
    ;; objects which will be printed using #N= syntax.  Delete those
    ;; objects which will be printed only once (to speed up assq later).
    (let ((rest circle-table)
	  (id -1))
      (while (cdr rest)
	(let ((tag (car (cdr rest))))
	  (cond ((cdr tag)
		 (setcdr tag id)
		 (setq id (1- id))
		 (setq rest (cdr rest)))
		;; Else delete this object.
		(t (setcdr rest (cdr (cdr rest))))))
	))
    ;; Drop the car.
    (cdr circle-table)
    ))



(defun cust-print-walk-circle-tree (object)
  (let (read-equivalent-p tag)
    (while object
      (setq read-equivalent-p
	    (or (numberp object)
		(and (symbolp object)
		     ;; Check if it is uninterned.
		     (eq object (intern-soft (symbol-name object)))))
	    tag (and (not read-equivalent-p)
		     (assq object (cdr circle-table))))
      (cond (tag
	     ;; Seen this object already, so note that.
	     (setcdr tag t))

	    ((not read-equivalent-p)
	     ;; Add a tag for this object.
	     (setcdr circle-table
		     (cons (list object)
			   (cdr circle-table)))))
      (setq object
	    (cond
	     (tag ;; No need to descend since we have already.
	      nil)

	     ((consp object)
	      ;; Walk the car of the list recursively.
	      (cust-print-walk-circle-tree (car object))
	      ;; But walk the cdr with the above while loop
	      ;; to avoid problems with max-lisp-eval-depth.
	      ;; And it should be faster than recursion.
	      (cdr object))

	     ((vectorp object)
	      ;; Walk the vector.
	      (let ((i (length object))
		    (j 0))
		(while (< j i)
		  (cust-print-walk-circle-tree (aref object j))
		  (setq j (1+ j))))))))))


;; Example.
;;=======================================

'(progn
   (progn
     ;; Create some circular structures.
     (setq circ-sym (let ((x (make-symbol "FOO"))) (list x x)))
     (setq circ-list (list 'a 'b (vector 1 2 3 4) 'd 'e 'f))
     (setcar (nthcdr 3 circ-list) circ-list)
     (aset (nth 2 circ-list) 2 circ-list)
     (setq dotted-circ-list (list 'a 'b 'c))
     (setcdr (cdr (cdr dotted-circ-list)) dotted-circ-list)
     (setq circ-vector (vector 1 2 3 4 (list 'a 'b 'c 'd) 6 7))
     (aset circ-vector 5 (make-symbol "-gensym-"))
     (setcar (cdr (aref circ-vector 4)) (aref circ-vector 5))
     nil)

   (install-custom-print)
   ;; (setq print-circle t)

   (let ((print-circle t))
     (or (equal (prin1-to-string circ-list) "#1=(a b [1 2 #1# 4] #1# e f)")
	 (error "circular object with array printing")))

   (let ((print-circle t))
     (or (equal (prin1-to-string dotted-circ-list) "#1=(a b c . #1#)")
	 (error "circular object with array printing")))

   (let* ((print-circle t)
	  (x (list 'p 'q))
	  (y (list (list 'a 'b) x 'foo x)))
     (setcdr (cdr (cdr (cdr y))) (cdr y))
     (or (equal (prin1-to-string y) "((a b) . #1=(#2=(p q) foo #2# . #1#))"
		)
	 (error "circular list example from CL manual")))

   (let ((print-circle nil))
     ;; cl-packages.el is required to print uninterned symbols like #:FOO.
     ;; (require 'cl-packages)
     (or (equal (prin1-to-string circ-sym) "(#:FOO #:FOO)")
	 (error "uninterned symbols in list")))
   (let ((print-circle t))
     (or (equal (prin1-to-string circ-sym) "(#1=FOO #1#)")
	 (error "circular uninterned symbols in list")))

   (uninstall-custom-print)
   )

(provide 'cust-print)

;;; cust-print.el ends here
