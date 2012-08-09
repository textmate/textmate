;;; xsd-regexp.el --- translate W3C XML Schema regexps to Emacs regexps

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, regexp

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

;; This handles the regular expressions in the syntax defined by:
;; W3C XML Schema Part 2: Datatypes
;; <http://www.w3.org/TR/xmlschema-2/#regexs>
;;
;; The main entry point is `xsdre-translate'.
;;
;; The features of XSD regexps that make this non-trivial are:
;;
;; - \p{PROP} escape for matching characters that have various
;;   Unicode-defined properties
;; - character class subtraction:, e.g. [\p{L}-[abc]] matches
;;   any character in the L category other than a, b and c.
;;
;; We compute the set of Unicode characters denoted by each XSD
;; char-class as a list of ranges.  The regexp generated for a
;; single escape can be large (several thousand characters).
;;
;; XSD has non-traditional rules about when characters must be
;; and can be quoted with \.  These are quite different from
;; the Emacs rules.
;;
;; The semantics of XSD regexps are defined in terms of Unicode.
;; Non-Unicode characters are not allowed in regular expressions and
;; will not match against the generated regular expressions.  A
;; Unicode character means a character in one of the Mule charsets
;; ascii, latin-iso8859-1, mule-unicode-0100-24ff,
;; mule-unicode-2500-33ff, mule-unicode-e000-ffff, eight-bit-control
;; or a character translatable to such a character (i.e a character
;; for which `encode-char' will return non-nil).
;;
;; Using unify-8859-on-decoding-mode is probably a good idea here
;; (and generally with XML and other Unicode-oriented formats).
;;
;; Unfortunately, this means that this package is currently useless
;; for CJK characters, since there's no mule-unicode charset for the
;; CJK ranges of Unicode.  We should devise a workaround for this
;; until the fabled Unicode version of Emacs makes an appearance.

;;; Code:

(defun xsdre-translate (regexp)
  "Translate a W3C XML Schema Datatypes regexp to an Emacs regexp.
Returns a string.  REGEXP is a string.  If REGEXP is not a valid XSD
regexp, signal an `xsdre-invalid-regexp' condition."
  (xsdre-from-symbolic
   (xsdre-to-symbolic regexp)))

(defvar xsdre-test-history nil)

(defun xsdre-test-regexp ()
  (interactive)
  (while
      (let* ((str (read-from-minibuffer "Regexp: "
					nil
					nil
					nil
					'xsdre-test-history))
	     (symbolic
	      (xsdre-to-symbolic str)))
	(with-output-to-temp-buffer "*XSD Regexp Test*"
	  (princ "XSD regexp: ")
	  (princ str)
	  (princ "\n")
	  (princ "Symbolic: ")
	  (princ "\n")
	  (pp symbolic)
	  (princ "\n")
	  (princ "Emacs regexp: ")
	  (princ (xsdre-from-symbolic symbolic)))
	t)))

;;; Range lists

(defsubst xsdre-make-range (first last)
  "Return a representation of a range of integers.
If the range contains a single integer, it is represented by that integer.
Otherwise, it is represented by a (FIRST . LAST) pair."
  (if (= first last)
      first
    (cons first last)))

(defsubst xsdre-range-first (r)
  "Return the first integer in a range."
  (if (consp r) (car r) r))

(defsubst xsdre-range-last (r)
  "Return the last integer in a range."
  (if (consp r) (cdr r) r))

(defun xsdre-make-range-list (list)
  "Make a range-list from a list of ranges.
A range-list represents a set of integers by a list of ranges in a
canonical form, in which ranges are in increasing order, and adjacent
ranges are merged wherever possible."
  (when list
    (setq list
	  (sort list 'xsdre-range-less-than))
    (let* ((next (cdr list))
	   (tail list)
	   (head (car list))
	   (first (xsdre-range-first head))
	   (last (xsdre-range-last head)))
      (while next
	(setq head (car next))
	(when (> (xsdre-range-last head) last)
	  (if (<= (xsdre-range-first head) (1+ last))
	      (setq last (xsdre-range-last head))
	    (setcar tail (xsdre-make-range first last))
	    (setcdr tail next)
	    (setq tail next)
	    (setq first (xsdre-range-first head))
	    (setq last (xsdre-range-last head))))
	(setq next (cdr next)))
      (setcar tail (xsdre-make-range first last))
      (setcdr tail nil)
      list)))


(defun xsdre-range-list-union (range-lists)
  "Return a range-list, the union of a list of range-lists."
  (xsdre-make-range-list (apply 'append range-lists)))

(defun xsdre-range-list-difference (orig subtract)
  "Return a range-list for the difference of two range-lists."
  (when orig
    (let (new head next first last)
      (while orig
	(setq head (car orig))
	(setq first (xsdre-range-first head))
	(setq last (xsdre-range-last head))
	(while (and subtract
		    (< (xsdre-range-last (car subtract)) first))
	  (setq subtract (cdr subtract)))
	(while (and subtract
		    (<= first last)
		    (<= (xsdre-range-first (car subtract)) last))
	  (when (< first (xsdre-range-first (car subtract)))
	    (setq new
		  (cons (xsdre-make-range
			 first
			 (1- (xsdre-range-first (car subtract))))
			new)))
	  (if (< (xsdre-range-last (car subtract)) last)
	      (progn
		(setq first (1+ (xsdre-range-last (car subtract))))
		(setq subtract (cdr subtract)))
	    (setq first (1+ last))))
	(when (<= first last)
	  (setq new (cons (xsdre-make-range first last) new)))
	(setq orig (cdr orig)))
      (nreverse new))))

(defun xsdre-range-less-than (r1 r2)
  "Return non-nil if range R1 is less than range R2."
  (or (< (xsdre-range-first r1) (xsdre-range-first r2))
      (and (= (xsdre-range-first r1) (xsdre-range-first r2))
	   (< (xsdre-range-last r1) (xsdre-range-last r2)))))

(defun xsdre-check-range-list (range-list)
  "Check that RANGE-LIST is a range-list.
Signal an error if it is not."
  (let ((last nil))
    (while range-list
      (unless (consp range-list)
	(error "Range list not a list"))
      (let ((head (car range-list)))
	(unless (or (integerp head)
		    (and (consp head)
			 (integerp (car head))
			 (integerp (cdr head))))
	  (error "Bad range %s" head))
	(when (and last
		   (not (< (1+ last) (xsdre-range-first head))))
	  (error "Ranges not strictly increasing"))
	(setq last (xsdre-range-last head)))
      (setq range-list (cdr range-list))))
  t)

;;; Compiling symbolic regexps to Emacs regexps

(defun xsdre-from-symbolic (re)
  "Return an Emacs regexp for the symbolic regexp RE."
  (apply 'concat
	 (nreverse (xsdre-compile-regexp re nil))))

(defun xsdre-compile-regexp (re accum)
  "Return a Emacs regular expression for the symbolic regexp RE.
Returns a list of strings whose head is the regexp for RE
and whose tail is ACCUM."
  (cond ((not (consp re))
	 (xsdre-compile-char-class re accum))
	((eq (car re) 'choice)
	 (setq accum (cons "\\(?:" accum))
	 (let ((choices (cdr re)))
	   (while choices
	     (setq accum
		   (xsdre-compile-regexp (car choices)
					 accum))
	     (setq choices (cdr choices))
	     (when choices
	       (setq accum
		     (cons "\\|" accum)))))
	 (cons "\\)" accum))
	((eq (car re) 'sequence)
	 (let ((members (cdr re)))
	   (while members
	     (setq accum (xsdre-compile-regexp (car members)
					       accum))
	     (setq members (cdr members))))
	 accum)
	((eq (car re) 'repeat)
	 (let* ((sub (nth 1 re))
		(lower (nth 2 re))
		(upper (nth 3 re))
		(need-paren (and (consp sub)
				 (eq (car sub) 'sequence))))
	   (when need-paren
	     (setq accum (cons "\\(?:" accum)))
	   (setq accum
		 (xsdre-compile-regexp sub accum))
	   (when need-paren
	     (setq accum (cons "\\)" accum)))
	   (cond ((not upper)
		  (cond ((eq lower 0)
			 (cons "*" accum))
			((eq lower 1)
			 (cons "+" accum))
			(t
			 (cons (concat "\\{"
				       (number-to-string lower)
				       ",\\}")
			       accum))))
		 ((eq lower upper)
		  (cons (concat "\\{"
				(number-to-string lower)
				"\\}")
			accum))
		 ((and (eq lower 0) (eq upper 1))
		  (cons "?" accum))
		 (t
		  (cons (concat "\\{"
				(number-to-string lower)
				","
				(number-to-string upper)
				"\\}")
			accum)))))
	(t (xsdre-compile-char-class re accum))))

(defun xsdre-compile-char-class (cc accum)
  "Return a Emacs regular expression for the symbolic character class CC.
Returns a list of strings whose head is the regexp for CC
and whose tail is ACCUM."
  (cons (if (integerp cc)
	    (xsdre-compile-single-char cc)
	  (let ((ranges (xsdre-range-list-mule-intersection
			 (xsdre-char-class-to-range-list cc))))
	    (cond ((null ranges) "\001-\000")
		  ((and (null (cdr ranges))
			(= (xsdre-range-first (car ranges))
			   (xsdre-range-last (car ranges))))
		   (xsdre-compile-single-char
		    (xsdre-range-first (car ranges))))
		  (t (xsdre-range-list-to-char-alternative ranges)))))
	accum))

(defun xsdre-compile-single-char (ch)
  (if (memq ch '(?. ?* ?+ ?? ?\[ ?\] ?^ ?$ ?\\))
      (string ?\\ ch)
    (string (decode-char 'ucs ch))))

(defun xsdre-char-class-to-range-list (cc)
  "Return a range-list for a symbolic char-class CC."
  (cond ((integerp cc) (list cc))
	((symbolp cc)
	 (or (get cc 'xsdre-ranges)
	     (xsdre-char-class-to-range-list (get cc 'xsdre-char-class))))
	((integerp (car cc))
	 (if (= (car cc) (cdr cc))
	     (car cc)
	   cc))
	((eq (car cc) 'union)
	 (xsdre-range-list-union (mapcar 'xsdre-char-class-to-range-list
					 (cdr cc))))
	((eq (car cc) 'difference)
	 (xsdre-range-list-difference
	  (xsdre-char-class-to-range-list (nth 1 cc))
	  (xsdre-char-class-to-range-list (nth 2 cc))))
	((eq (car cc) 'range)
	 (list (xsdre-make-range (nth 1 cc) (nth 2 cc))))
	(t (error "Internal error in XSD regexp compilation: \
unknown char-class %s" cc))))

(defconst xsdre-mule-char-set-ranges
  '((0 . 127)
    (128 . 159)
    (160 . 255)
    (#x0100 . #x24ff)
    (#x2500 . #x33ff)
    (#xe000 . #xffff))
  "List of ranges for the Mule character sets containing Unicode characters.")

(defun xsdre-range-list-mule-intersection (range-list)
  "Return the intersection of RANGE-LIST with the mule-supported ranges.
Also split ranges so that no range spans more that one mule charset."
  (when range-list
    (let* ((char-set-ranges (cdr xsdre-mule-char-set-ranges))
	   (mule-ranges nil)
	   (char-set-first (caar xsdre-mule-char-set-ranges))
	   (char-set-last (cdar xsdre-mule-char-set-ranges))
	   (range (car range-list))
	   (first (xsdre-range-first range))
	   (last (xsdre-range-last range)))
      (setq range-list (cdr range-list))
      (while (progn
	       (cond ((> first last)
		      (if (null range-list)
			  nil
			(setq range (car range-list))
			(setq first (xsdre-range-first range))
			(setq last (xsdre-range-last range))
			(setq range-list (cdr range-list))
			t))
		     ((< char-set-last first)
		      (if (null char-set-ranges)
			  nil
			(setq char-set-first (caar char-set-ranges))
			(setq char-set-last (cdar char-set-ranges))
			(setq char-set-ranges (cdr char-set-ranges))
			t))
		     ((< first char-set-first)
		      (setq first char-set-first))
		     ;; Now we know that
		     ;; first <= last
		     ;; first <= char-set-last
		     ;; first >= char-set-first
		     ((<= last char-set-last)
		      (setq mule-ranges
			    (cons (xsdre-make-range first last)
				  mule-ranges))
		      (setq first (1+ last))
		      t)
		     (t
		      (setq mule-ranges
			    (cons (xsdre-make-range first char-set-last)
				  mule-ranges))
		      (setq first (1+ char-set-last))
		      t))))
      (nreverse mule-ranges))))

(defun xsdre-range-list-to-char-alternative (range-list)
  "Return a char alternative for a range-list.
RANGE-LIST must contain more than integer.
The char alternative is a string containing an Emacs regexp
consisting of a single char alternative delimited with []."
  (let (range caret close-bracket hyphen chars first last)
    (while range-list
      (setq range (car range-list))
      (setq first (xsdre-range-first range))
      (setq last (xsdre-range-last range))
      (while (and (cond ((eq first ?^)
			 (setq caret t)
			 (setq first (1+ first)))
			((eq first ?-)
			 (setq hyphen t)
			 (setq first (1+ first)))
			((eq first ?\])
			 (setq close-bracket t)
			 (setq first (1+ first))))
		  (<= first last)))
      (when (<= first last)
	(setq chars
	      (cons first chars))
	(when (< first last)
	  (setq chars
		(if (and (eq last (1+ first))
			 (not (eq last ?-)))
		    (cons last chars)
		  (cons last (cons ?- chars))))))
      (setq range-list (cdr range-list)))
    (setq chars
	  (mapcar (lambda (c)
		    (decode-char 'ucs c))
		  chars))
    (when caret
      (setq chars (cons ?^ chars)))
    (when hyphen
      (setq chars (cons ?- chars)))
    (setq chars (cons ?\] chars))
    (setq chars (nreverse chars))
    (when close-bracket
      (setq chars (cons ?\] chars)))
    (when (equal chars '(?^ ?- ?\]))
      (setq chars '(?- ?^ ?\])))
    (setq chars (cons ?\[ chars))
    (apply 'string chars)))

;;; Parsing

(defvar xsdre-current-regexp nil
  "List of characters remaining to be parsed.  Dynamically bound.")

(defun xsdre-to-symbolic (str)
  "Convert a W3C XML Schema datatypes regexp to a symbolic form.

The symbolic form has the following structure:

REGEXP ::=
  (sequence REGEXP ...)
  | (choice REGEXP ...)
  | (repeat REGEXP MIN MAX)
  | CHAR-CLASS

CHAR-CLASS ::=
  CHAR
  | SYMBOLIC-CHAR-CLASS
  | RANGE
  | (union CHAR-CLASS ...)
  | (difference CHAR-CLASS CHAR-CLASS)

RANGE ::= (range LOWER UPPER)

MIN ::= INTEGER
MAX ::= INTEGER | nil
CHAR ::= UNICODE
LOWER ::= UNICODE
UPPER ::= UNICODE
SYMBOLIC-CHAR-CLASS ::= SYMBOL

where UNICODE is a integer specifying a Unicode code-point and
SYMBOLIC-CHAR-CLASS is a symbol which has either a `xsdre-char-class'
property whose value is a CHAR-CLASS, or a `xsdre-ranges' property
whose value is a range-list."
  (let ((xsdre-current-regexp (string-to-list str)))
    (condition-case err
	(let ((symbolic (xsdre-parse-regexp)))
	  (if xsdre-current-regexp
	      (xsdre-parse-error "Unexpected %c" (car xsdre-current-regexp))
	    symbolic))
      (xsdre-parse-error
       (signal 'xsdre-invalid-regexp
	       (list (apply 'format (cdr err))
		     (- (length str)
			(length xsdre-current-regexp))))))))

(put 'xsdre-invalid-regexp
     'error-conditions
     '(error xsdre-invalid-regexp))

(put 'xsdre-invalid-regexp
     'error-message
     "Invalid W3C XML Schema Datatypes regular expression")

(defun xsdre-parse-regexp ()
  (let ((branches nil))
    (while (progn
	     (setq branches (cons (xsdre-parse-branch) branches))
	     (when (eq (car xsdre-current-regexp) ?|)
	       (xsdre-advance)
	       t)))
    (if (null (cdr branches))
	(car branches)
      (cons 'choice (nreverse branches)))))

(defun xsdre-parse-branch ()
  (let (items)
    (while (let ((item (xsdre-try-parse-atom)))
	     (when item
	       (let ((quantifier (xsdre-try-parse-quantifier)))
		 (when quantifier
		   (setq item
			 (list 'repeat
			       item
			       (car quantifier)
			       (cdr quantifier)))))
	       (setq items (cons item items)))))
    (cond ((null items) '(sequence))
	  ((null (cdr items)) (car items))
	  (t (cons 'sequence (nreverse items))))))

(defun xsdre-try-parse-quantifier ()
  (let ((ch (car xsdre-current-regexp)))
    (cond ((eq ch ?*) (xsdre-advance) '(0 . nil))
	  ((eq ch ?+) (xsdre-advance) '(1 . nil))
	  ((eq ch ??) (xsdre-advance) '(0 . 1))
	  ((eq ch ?{)
	   (xsdre-advance)
	   (let ((lower (xsdre-parse-bound)))
	     (setq ch (car xsdre-current-regexp))
	     (cond ((eq ch ?})
		    (xsdre-advance)
		    (cons lower lower))
		   ((eq ch ?,)
		    (xsdre-advance)
		    (cond ((eq (car xsdre-current-regexp) ?})
			   (xsdre-advance)
			   (cons lower nil))
			  (t
			   (let ((upper (xsdre-parse-bound)))
			     (xsdre-expect ?})
			     (cons lower upper)))))
		   (t (xsdre-parse-error "Expected , or }")))))
	  (t nil))))

(defun xsdre-parse-bound ()
  (let ((n 0))
    (while (progn
	     (let* ((ch (car xsdre-current-regexp))
		    (digit (memq ch '(?9 ?8 ?7 ?6 ?5 ?4 ?3 ?2 ?1 ?0))))
	       (unless digit
		 (xsdre-parse-error "Expected a digit"))
	       (setq n (+ (* n 10)
			  (length (cdr digit)))))
	     (xsdre-advance)
	     (not (memq (car xsdre-current-regexp) '(?} ?,)))))
    n))


(defun xsdre-try-parse-atom ()
  (let ((ch (car xsdre-current-regexp)))
    (cond  ((memq ch '(nil ?? ?* ?+ ?\) ?\{ ?\} ?| ?\])) nil)
	   ((eq ch ?\\)
	    (xsdre-advance)
	    (xsdre-parse-escape))
	   ((eq ch ?\()
	    (xsdre-advance)
	    (let ((ret (xsdre-parse-regexp)))
	      (xsdre-expect ?\))
	      ret))
	   ((eq ch ?\[)
	    (xsdre-parse-char-class))
	   ((eq ch ?.)
	    (xsdre-advance)
	    'dot)
	   (t
	    (let ((uc (encode-char ch 'ucs)))
	      (unless uc
		(xsdre-parse-error "%c is not a Unicode character" ch))
	      (xsdre-advance) uc)))))

(defun xsdre-parse-char-class ()
  (xsdre-advance)
  (let (compl members ret)
    (when (eq (car xsdre-current-regexp) ?^)
      (setq compl t)
      (xsdre-advance))
    (while (let ((member (xsdre-parse-char-class-member))
		 uc1 uc2)
	     (cond ((eq (car xsdre-current-regexp) ?\-)
		    (xsdre-advance)
		    (cond ((eq (car xsdre-current-regexp) ?\[)
			   (setq members (cons member members))
			   nil)
			  ((not (integerp member))
			   (xsdre-parse-error "Lower bound is not a single character"))
			  ((not (setq uc1
				      (encode-char member 'ucs)))
			   (xsdre-parse-error "Lower bound %c is not a Unicode character"
				  member))
			  (t
			   (let ((upper (xsdre-parse-char-class-member)))
			     (unless (integerp upper)
			       (xsdre-parse-error "Upper bound is not a single character"))
			     (unless (setq uc2
					   (encode-char upper 'ucs))
			       (xsdre-parse-error "Upper bound %c is not a Unicode character" upper))
			     (setq members
				   (cons (list 'range uc1 uc2)
					 members)))
			   (not (eq (car xsdre-current-regexp) ?\])))))
		   (t (setq members (cons member members))
		      (not (eq (car xsdre-current-regexp) ?\]))))))
    (setq members (nreverse members))
    (if (null (cdr members))
	(setq ret (car members))
      (setq ret (cons 'union members)))
    (when compl
      (setq ret (list 'difference 'any ret)))
    (when (eq (car xsdre-current-regexp) ?\[)
      (setq ret
	    (list 'difference ret (xsdre-parse-char-class))))
    (xsdre-expect ?\])
    ret))

(defun xsdre-parse-char-class-member ()
  (let ((ch (car xsdre-current-regexp)))
    (cond ((null ch)
	   (xsdre-parse-error "Expected ]"))
	  ((eq ch ?\\)
	   (xsdre-advance)
	   (xsdre-parse-escape))
	  ((memq ch '(?\[ ?\] ?-))
	   (xsdre-parse-error "%c must be quoted in a character class" ch))
	  (t (xsdre-advance) ch))))

(defconst xsdre-single-escape
  '((?s . space)
    (?i . name-initial)
    (?c . name-continue)
    (?d . digit)
    (?w . word)))

(defun xsdre-parse-escape ()
  (let ((ch (car xsdre-current-regexp)))
    (xsdre-advance)
    (cond ((memq ch '(?\\ ?| ?. ?- ?^ ?* ?+ ?( ?) ?{ ?} ?[ ?])) ch)
	  ((eq ch ?r) ?\r)
	  ((eq ch ?n) ?\n)
	  ((eq ch ?t) ?\t)
	  ((cdr (assq ch xsdre-single-escape)))
	  ((let ((positive
		  (cdr (assq (downcase ch) xsdre-single-escape))))
	     (and positive
		  (list 'difference 'any positive))))
	  ((eq ch ?p) (xsdre-parse-prop))
	  ((eq ch ?P) (list 'difference 'any (xsdre-parse-prop)))
	  (t (if ch
		 (xsdre-parse-error "Missing char after \\")
	       (xsdre-parse-error "Bad escape %c" ch))))))

(defun xsdre-parse-prop ()
  (xsdre-expect ?{)
  (let ((name nil))
    (while (not (eq (car xsdre-current-regexp) ?\}))
      (unless xsdre-current-regexp
	(xsdre-parse-error "Expected ?"))
      (setq name (cons (car xsdre-current-regexp)
		       name))
      (xsdre-advance))
    (xsdre-advance)
    (setq name (nreverse name))
    (cond ((null name) (xsdre-parse-error "Empty property name"))
	  ((null (cdr name))
	   (let ((category (intern (string (car name)))))
	     (unless (get category 'xsdre-unicode-category)
	       (xsdre-parse-error "%s is not a category" category))
	     category))
	  ((null (cddr name))
	   (let ((category (intern (string (car name) (cadr name)))))
	     (unless (get category 'xsdre-unicode-category)
	       (xsdre-parse-error "%s is not a category" category))
	     category))
	  ((not (and (eq (car name) ?I)
		     (eq (cadr name) ?s)))
	   (xsdre-parse-error "Block name does not start with Is"))
	  (t
	   (let ((block (intern (apply 'string (cddr name)))))
	     (unless (get block 'xsdre-unicode-block)
	       (xsdre-parse-error "%s is not a block name" block))
	     block)))))

(defun xsdre-expect (ch)
  (if (eq (car xsdre-current-regexp) ch)
      (xsdre-advance)
    (xsdre-parse-error "Expected %c" ch)))

(defun xsdre-advance ()
  (setq xsdre-current-regexp
	(cdr xsdre-current-regexp)))

(defun xsdre-parse-error (&rest args)
  (signal 'xsdre-parse-error args))

;; This error condition is used only internally.

(put 'xsdre-parse-error
     'error-conditions
     '(error xsdre-parse-error))

(put 'xsdre-parse-error
     'error-message
     "Internal error in parsing XSD regexp")

;;; Character class data

(put 'dot 'xsdre-char-class '(difference any (union #xA #xD)))
(put 'digit 'xsdre-char-class 'Nd)
(put 'word 'xsdre-char-class '(difference any (union P Z C)))
(put 'space 'xsdre-char-class '(union #x9 #xA #xD #x20))
(put 'any 'xsdre-ranges '((#x0 . #x10FFFF)))

(defconst xsdre-gen-categories
  '(Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd
       Ps Pe Pi Pf Po Zs Zl Zp Sm Sc Sk So Cc Cf Co))

(defun xsdre-gen-categories (file)
  "Use a UnicodeData file to generate code to initialize Unicode categories.
Code is inserted into the current buffer."
  (interactive "fUnicodeData file: ")
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (mapc (lambda (x) (put x 'xsdre-ranges nil)) xsdre-gen-categories)
    (while (re-search-forward "^\\([0-9A-Fa-f]*\\);[^;]*;\\([A-Z][a-z]\\);"
			      nil
			      t)
      (let* ((sym (intern (match-string-no-properties 2)))
	     (code (string-to-number (match-string-no-properties 1)
				     16))
	     (ranges (get sym 'xsdre-ranges))
	     (last-range (car ranges))
	     (forced-range (string= (buffer-substring-no-properties
				     (- (match-beginning 2) 6)
				     (1- (match-beginning 2)))
				    "Last>")))
	(cond ((and (integerp last-range)
		    (or forced-range
			(eq code (1+ last-range))))
	       (put sym
		    'xsdre-ranges
		    (cons (cons last-range code)
			  (cdr ranges))))
	      ((and (consp last-range)
		    (or forced-range
			(eq code (1+ (cdr last-range)))))
	       (put sym
		    'xsdre-ranges
		    (cons (cons (car last-range) code)
			  (cdr ranges))))
	      (t
	       (put sym 'xsdre-ranges (cons code ranges))))))
    (mapc (lambda (x)
            (put x
                 'xsdre-ranges
                 (nreverse (get x 'xsdre-ranges)))
            nil)
          xsdre-gen-categories))
  (mapc (lambda (x)
          (let ((start (point)))
            (pp (list 'xsdre-def-primitive-category
                      (list 'quote x)
                      (list 'quote (get x 'xsdre-ranges)))
                (current-buffer))
            (save-excursion
              (goto-char start)
              (down-list 2)
              (while (condition-case err
                         (progn
                           (forward-sexp)
                           t)
                       (error nil))
                (when (and (< 70 (current-column))
                           (not (looking-at ")")))
                  (insert "\n")
                  (lisp-indent-line))))))
        xsdre-gen-categories))

(defun xsdre-def-primitive-category (sym ranges)
  (put sym 'xsdre-ranges ranges)
  (put sym 'xsdre-unicode-category t))

;;; Blocks

(defun xsdre-def-block (sym ranges)
  (put sym 'xsdre-ranges ranges)
  (put sym 'xsdre-unicode-block t))

(xsdre-def-block 'BasicLatin '((#x0000 . #x007F)))
(xsdre-def-block 'Latin-1Supplement '((#x0080 . #x00FF)))
(xsdre-def-block 'LatinExtended-A '((#x0100 . #x017F)))
(xsdre-def-block 'LatinExtended-B '((#x0180 . #x024F)))
(xsdre-def-block 'IPAExtensions '((#x0250 . #x02AF)))
(xsdre-def-block 'SpacingModifierLetters '((#x02B0 . #x02FF)))
(xsdre-def-block 'CombiningDiacriticalMarks '((#x0300 . #x036F)))
(xsdre-def-block 'Greek '((#x0370 . #x03FF)))
(xsdre-def-block 'Cyrillic '((#x0400 . #x04FF)))
(xsdre-def-block 'Armenian '((#x0530 . #x058F)))
(xsdre-def-block 'Hebrew '((#x0590 . #x05FF)))
(xsdre-def-block 'Arabic '((#x0600 . #x06FF)))
(xsdre-def-block 'Syriac '((#x0700 . #x074F)))
(xsdre-def-block 'Thaana '((#x0780 . #x07BF)))
(xsdre-def-block 'Devanagari '((#x0900 . #x097F)))
(xsdre-def-block 'Bengali '((#x0980 . #x09FF)))
(xsdre-def-block 'Gurmukhi '((#x0A00 . #x0A7F)))
(xsdre-def-block 'Gujarati '((#x0A80 . #x0AFF)))
(xsdre-def-block 'Oriya '((#x0B00 . #x0B7F)))
(xsdre-def-block 'Tamil '((#x0B80 . #x0BFF)))
(xsdre-def-block 'Telugu '((#x0C00 . #x0C7F)))
(xsdre-def-block 'Kannada '((#x0C80 . #x0CFF)))
(xsdre-def-block 'Malayalam '((#x0D00 . #x0D7F)))
(xsdre-def-block 'Sinhala '((#x0D80 . #x0DFF)))
(xsdre-def-block 'Thai '((#x0E00 . #x0E7F)))
(xsdre-def-block 'Lao '((#x0E80 . #x0EFF)))
(xsdre-def-block 'Tibetan '((#x0F00 . #x0FFF)))
(xsdre-def-block 'Myanmar '((#x1000 . #x109F)))
(xsdre-def-block 'Georgian '((#x10A0 . #x10FF)))
(xsdre-def-block 'HangulJamo '((#x1100 . #x11FF)))
(xsdre-def-block 'Ethiopic '((#x1200 . #x137F)))
(xsdre-def-block 'Cherokee '((#x13A0 . #x13FF)))
(xsdre-def-block 'UnifiedCanadianAboriginalSyllabics '((#x1400 . #x167F)))
(xsdre-def-block 'Ogham '((#x1680 . #x169F)))
(xsdre-def-block 'Runic '((#x16A0 . #x16FF)))
(xsdre-def-block 'Khmer '((#x1780 . #x17FF)))
(xsdre-def-block 'Mongolian '((#x1800 . #x18AF)))
(xsdre-def-block 'LatinExtendedAdditional '((#x1E00 . #x1EFF)))
(xsdre-def-block 'GreekExtended '((#x1F00 . #x1FFF)))
(xsdre-def-block 'GeneralPunctuation '((#x2000 . #x206F)))
(xsdre-def-block 'SuperscriptsandSubscripts '((#x2070 . #x209F)))
(xsdre-def-block 'CurrencySymbols '((#x20A0 . #x20CF)))
(xsdre-def-block 'CombiningMarksforSymbols '((#x20D0 . #x20FF)))
(xsdre-def-block 'LetterlikeSymbols '((#x2100 . #x214F)))
(xsdre-def-block 'NumberForms '((#x2150 . #x218F)))
(xsdre-def-block 'Arrows '((#x2190 . #x21FF)))
(xsdre-def-block 'MathematicalOperators '((#x2200 . #x22FF)))
(xsdre-def-block 'MiscellaneousTechnical '((#x2300 . #x23FF)))
(xsdre-def-block 'ControlPictures '((#x2400 . #x243F)))
(xsdre-def-block 'OpticalCharacterRecognition '((#x2440 . #x245F)))
(xsdre-def-block 'EnclosedAlphanumerics '((#x2460 . #x24FF)))
(xsdre-def-block 'BoxDrawing '((#x2500 . #x257F)))
(xsdre-def-block 'BlockElements '((#x2580 . #x259F)))
(xsdre-def-block 'GeometricShapes '((#x25A0 . #x25FF)))
(xsdre-def-block 'MiscellaneousSymbols '((#x2600 . #x26FF)))
(xsdre-def-block 'Dingbats '((#x2700 . #x27BF)))
(xsdre-def-block 'BraillePatterns '((#x2800 . #x28FF)))
(xsdre-def-block 'CJKRadicalsSupplement '((#x2E80 . #x2EFF)))
(xsdre-def-block 'KangxiRadicals '((#x2F00 . #x2FDF)))
(xsdre-def-block 'IdeographicDescriptionCharacters '((#x2FF0 . #x2FFF)))
(xsdre-def-block 'CJKSymbolsandPunctuation '((#x3000 . #x303F)))
(xsdre-def-block 'Hiragana '((#x3040 . #x309F)))
(xsdre-def-block 'Katakana '((#x30A0 . #x30FF)))
(xsdre-def-block 'Bopomofo '((#x3100 . #x312F)))
(xsdre-def-block 'HangulCompatibilityJamo '((#x3130 . #x318F)))
(xsdre-def-block 'Kanbun '((#x3190 . #x319F)))
(xsdre-def-block 'BopomofoExtended '((#x31A0 . #x31BF)))
(xsdre-def-block 'EnclosedCJKLettersandMonths '((#x3200 . #x32FF)))
(xsdre-def-block 'CJKCompatibility '((#x3300 . #x33FF)))
(xsdre-def-block 'CJKUnifiedIdeographsExtensionA '((#x3400 . #x4DB5)))
(xsdre-def-block 'CJKUnifiedIdeographs '((#x4E00 . #x9FFF)))
(xsdre-def-block 'YiSyllables '((#xA000 . #xA48F)))
(xsdre-def-block 'YiRadicals '((#xA490 . #xA4CF)))
(xsdre-def-block 'HangulSyllables '((#xAC00 . #xD7A3)))
;;(xsdre-def-block 'HighSurrogates '((#xD800 . #xDB7F)))
;;(xsdre-def-block 'HighPrivateUseSurrogates '((#xDB80 . #xDBFF)))
;;(xsdre-def-block 'LowSurrogates '((#xDC00 . #xDFFF)))
(xsdre-def-block 'CJKCompatibilityIdeographs '((#xF900 . #xFAFF)))
(xsdre-def-block 'AlphabeticPresentationForms '((#xFB00 . #xFB4F)))
(xsdre-def-block 'ArabicPresentationForms-A '((#xFB50 . #xFDFF)))
(xsdre-def-block 'CombiningHalfMarks '((#xFE20 . #xFE2F)))
(xsdre-def-block 'CJKCompatibilityForms '((#xFE30 . #xFE4F)))
(xsdre-def-block 'SmallFormVariants '((#xFE50 . #xFE6F)))
(xsdre-def-block 'ArabicPresentationForms-B '((#xFE70 . #xFEFE)))
(xsdre-def-block 'Specials '((#xFEFF . #xFEFF)))
(xsdre-def-block 'HalfwidthandFullwidthForms '((#xFF00 . #xFFEF)))
(xsdre-def-block 'Specials '((#xFFF0 . #xFFFD)))
(xsdre-def-block 'OldItalic '((#x10300 . #x1032F)))
(xsdre-def-block 'Gothic '((#x10330 . #x1034F)))
(xsdre-def-block 'Deseret '((#x10400 . #x1044F)))
(xsdre-def-block 'ByzantineMusicalSymbols '((#x1D000 . #x1D0FF)))
(xsdre-def-block 'MusicalSymbols '((#x1D100 . #x1D1FF)))
(xsdre-def-block 'MathematicalAlphanumericSymbols '((#x1D400 . #x1D7FF)))
(xsdre-def-block 'CJKUnifiedIdeographsExtensionB '((#x20000 . #x2A6D6)))
(xsdre-def-block 'CJKCompatibilityIdeographsSupplement '((#x2F800 . #x2FA1F)))
(xsdre-def-block 'Tags '((#xE0000 . #xE007F)))
(xsdre-def-block 'PrivateUse '((#xE000 . #xF8FF)
			       (#xF0000 . #xFFFFD)
			       (#x100000 . #x10FFFD)))

;;; Categories

;;; Derived categories

(defun xsdre-def-derived-category (sym char-class)
  (put sym 'xsdre-char-class char-class)
  (put sym 'xsdre-unicode-category t))

(xsdre-def-derived-category 'L '(union Lu Ll Lt Lm Lo))
(xsdre-def-derived-category 'M '(union Mn Mc Me))
(xsdre-def-derived-category 'N '(union Nd Nl No))
(xsdre-def-derived-category 'P '(union Pc Pd Ps Pe Pi Pf Po))
(xsdre-def-derived-category 'Z '(union Zs Zl Zp))
(xsdre-def-derived-category 'S '(union Sm Sc Sk So))
(xsdre-def-derived-category 'C '(union Cc Cf Co Cn))
(xsdre-def-derived-category 'Cn '(difference any
					     (union L M N P Z S Cc Cf Co)))

(xsdre-def-primitive-category
 'name-initial
 '(#x003a
   (#x0041 . #x005a)
   #x005f
   (#x0061 . #x007a)
   (#x00c0 . #x00d6)
   (#x00d8 . #x00f6)
   (#x00f8 . #x0131)
   (#x0134 . #x013e)
   (#x0141 . #x0148)
   (#x014a . #x017e)
   (#x0180 . #x01c3)
   (#x01cd . #x01f0)
   (#x01f4 . #x01f5)
   (#x01fa . #x0217)
   (#x0250 . #x02a8)
   (#x02bb . #x02c1)
   #x0386
   (#x0388 . #x038a)
   #x038c
   (#x038e . #x03a1)
   (#x03a3 . #x03ce)
   (#x03d0 . #x03d6)
   #x03da
   #x03dc
   #x03de
   #x03e0
   (#x03e2 . #x03f3)
   (#x0401 . #x040c)
   (#x040e . #x044f)
   (#x0451 . #x045c)
   (#x045e . #x0481)
   (#x0490 . #x04c4)
   (#x04c7 . #x04c8)
   (#x04cb . #x04cc)
   (#x04d0 . #x04eb)
   (#x04ee . #x04f5)
   (#x04f8 . #x04f9)
   (#x0531 . #x0556)
   #x0559
   (#x0561 . #x0586)
   (#x05d0 . #x05ea)
   (#x05f0 . #x05f2)
   (#x0621 . #x063a)
   (#x0641 . #x064a)
   (#x0671 . #x06b7)
   (#x06ba . #x06be)
   (#x06c0 . #x06ce)
   (#x06d0 . #x06d3)
   #x06d5
   (#x06e5 . #x06e6)
   (#x0905 . #x0939)
   #x093d
   (#x0958 . #x0961)
   (#x0985 . #x098c)
   (#x098f . #x0990)
   (#x0993 . #x09a8)
   (#x09aa . #x09b0)
   #x09b2
   (#x09b6 . #x09b9)
   (#x09dc . #x09dd)
   (#x09df . #x09e1)
   (#x09f0 . #x09f1)
   (#x0a05 . #x0a0a)
   (#x0a0f . #x0a10)
   (#x0a13 . #x0a28)
   (#x0a2a . #x0a30)
   (#x0a32 . #x0a33)
   (#x0a35 . #x0a36)
   (#x0a38 . #x0a39)
   (#x0a59 . #x0a5c)
   #x0a5e
   (#x0a72 . #x0a74)
   (#x0a85 . #x0a8b)
   #x0a8d
   (#x0a8f . #x0a91)
   (#x0a93 . #x0aa8)
   (#x0aaa . #x0ab0)
   (#x0ab2 . #x0ab3)
   (#x0ab5 . #x0ab9)
   #x0abd
   #x0ae0
   (#x0b05 . #x0b0c)
   (#x0b0f . #x0b10)
   (#x0b13 . #x0b28)
   (#x0b2a . #x0b30)
   (#x0b32 . #x0b33)
   (#x0b36 . #x0b39)
   #x0b3d
   (#x0b5c . #x0b5d)
   (#x0b5f . #x0b61)
   (#x0b85 . #x0b8a)
   (#x0b8e . #x0b90)
   (#x0b92 . #x0b95)
   (#x0b99 . #x0b9a)
   #x0b9c
   (#x0b9e . #x0b9f)
   (#x0ba3 . #x0ba4)
   (#x0ba8 . #x0baa)
   (#x0bae . #x0bb5)
   (#x0bb7 . #x0bb9)
   (#x0c05 . #x0c0c)
   (#x0c0e . #x0c10)
   (#x0c12 . #x0c28)
   (#x0c2a . #x0c33)
   (#x0c35 . #x0c39)
   (#x0c60 . #x0c61)
   (#x0c85 . #x0c8c)
   (#x0c8e . #x0c90)
   (#x0c92 . #x0ca8)
   (#x0caa . #x0cb3)
   (#x0cb5 . #x0cb9)
   #x0cde
   (#x0ce0 . #x0ce1)
   (#x0d05 . #x0d0c)
   (#x0d0e . #x0d10)
   (#x0d12 . #x0d28)
   (#x0d2a . #x0d39)
   (#x0d60 . #x0d61)
   (#x0e01 . #x0e2e)
   #x0e30
   (#x0e32 . #x0e33)
   (#x0e40 . #x0e45)
   (#x0e81 . #x0e82)
   #x0e84
   (#x0e87 . #x0e88)
   #x0e8a
   #x0e8d
   (#x0e94 . #x0e97)
   (#x0e99 . #x0e9f)
   (#x0ea1 . #x0ea3)
   #x0ea5
   #x0ea7
   (#x0eaa . #x0eab)
   (#x0ead . #x0eae)
   #x0eb0
   (#x0eb2 . #x0eb3)
   #x0ebd
   (#x0ec0 . #x0ec4)
   (#x0f40 . #x0f47)
   (#x0f49 . #x0f69)
   (#x10a0 . #x10c5)
   (#x10d0 . #x10f6)
   #x1100
   (#x1102 . #x1103)
   (#x1105 . #x1107)
   #x1109
   (#x110b . #x110c)
   (#x110e . #x1112)
   #x113c
   #x113e
   #x1140
   #x114c
   #x114e
   #x1150
   (#x1154 . #x1155)
   #x1159
   (#x115f . #x1161)
   #x1163
   #x1165
   #x1167
   #x1169
   (#x116d . #x116e)
   (#x1172 . #x1173)
   #x1175
   #x119e
   #x11a8
   #x11ab
   (#x11ae . #x11af)
   (#x11b7 . #x11b8)
   #x11ba
   (#x11bc . #x11c2)
   #x11eb
   #x11f0
   #x11f9
   (#x1e00 . #x1e9b)
   (#x1ea0 . #x1ef9)
   (#x1f00 . #x1f15)
   (#x1f18 . #x1f1d)
   (#x1f20 . #x1f45)
   (#x1f48 . #x1f4d)
   (#x1f50 . #x1f57)
   #x1f59
   #x1f5b
   #x1f5d
   (#x1f5f . #x1f7d)
   (#x1f80 . #x1fb4)
   (#x1fb6 . #x1fbc)
   #x1fbe
   (#x1fc2 . #x1fc4)
   (#x1fc6 . #x1fcc)
   (#x1fd0 . #x1fd3)
   (#x1fd6 . #x1fdb)
   (#x1fe0 . #x1fec)
   (#x1ff2 . #x1ff4)
   (#x1ff6 . #x1ffc)
   #x2126
   (#x212a . #x212b)
   #x212e
   (#x2180 . #x2182)
   #x3007
   (#x3021 . #x3029)
   (#x3041 . #x3094)
   (#x30a1 . #x30fa)
   (#x3105 . #x312c)
   (#x4e00 . #x9fa5)
   (#xac00 . #xd7a3)))

(xsdre-def-derived-category 'name-continue '(union name-initial
						   name-continue-not-initial))

(xsdre-def-primitive-category
 'name-continue-not-initial
 '((#x002d . #x002e)
   (#x0030 . #x0039)
   #x00b7
   (#x02d0 . #x02d1)
   (#x0300 . #x0345)
   (#x0360 . #x0361)
   #x0387
   (#x0483 . #x0486)
   (#x0591 . #x05a1)
   (#x05a3 . #x05b9)
   (#x05bb . #x05bd)
   #x05bf
   (#x05c1 . #x05c2)
   #x05c4
   #x0640
   (#x064b . #x0652)
   (#x0660 . #x0669)
   #x0670
   (#x06d6 . #x06dc)
   (#x06dd . #x06df)
   (#x06e0 . #x06e4)
   (#x06e7 . #x06e8)
   (#x06ea . #x06ed)
   (#x06f0 . #x06f9)
   (#x0901 . #x0903)
   #x093c
   (#x093e . #x094c)
   #x094d
   (#x0951 . #x0954)
   (#x0962 . #x0963)
   (#x0966 . #x096f)
   (#x0981 . #x0983)
   #x09bc
   (#x09be . #x09bf)
   (#x09c0 . #x09c4)
   (#x09c7 . #x09c8)
   (#x09cb . #x09cd)
   #x09d7
   (#x09e2 . #x09e3)
   (#x09e6 . #x09ef)
   #x0a02
   #x0a3c
   (#x0a3e . #x0a42)
   (#x0a47 . #x0a48)
   (#x0a4b . #x0a4d)
   (#x0a66 . #x0a6f)
   (#x0a70 . #x0a71)
   (#x0a81 . #x0a83)
   #x0abc
   (#x0abe . #x0ac5)
   (#x0ac7 . #x0ac9)
   (#x0acb . #x0acd)
   (#x0ae6 . #x0aef)
   (#x0b01 . #x0b03)
   #x0b3c
   (#x0b3e . #x0b43)
   (#x0b47 . #x0b48)
   (#x0b4b . #x0b4d)
   (#x0b56 . #x0b57)
   (#x0b66 . #x0b6f)
   (#x0b82 . #x0b83)
   (#x0bbe . #x0bc2)
   (#x0bc6 . #x0bc8)
   (#x0bca . #x0bcd)
   #x0bd7
   (#x0be7 . #x0bef)
   (#x0c01 . #x0c03)
   (#x0c3e . #x0c44)
   (#x0c46 . #x0c48)
   (#x0c4a . #x0c4d)
   (#x0c55 . #x0c56)
   (#x0c66 . #x0c6f)
   (#x0c82 . #x0c83)
   (#x0cbe . #x0cc4)
   (#x0cc6 . #x0cc8)
   (#x0cca . #x0ccd)
   (#x0cd5 . #x0cd6)
   (#x0ce6 . #x0cef)
   (#x0d02 . #x0d03)
   (#x0d3e . #x0d43)
   (#x0d46 . #x0d48)
   (#x0d4a . #x0d4d)
   #x0d57
   (#x0d66 . #x0d6f)
   #x0e31
   (#x0e34 . #x0e3a)
   (#x0e46 . #x0e4e)
   (#x0e50 . #x0e59)
   #x0eb1
   (#x0eb4 . #x0eb9)
   (#x0ebb . #x0ebc)
   #x0ec6
   (#x0ec8 . #x0ecd)
   (#x0ed0 . #x0ed9)
   (#x0f18 . #x0f19)
   (#x0f20 . #x0f29)
   #x0f35
   #x0f37
   #x0f39
   (#x0f3e . #x0f3f)
   (#x0f71 . #x0f84)
   (#x0f86 . #x0f8b)
   (#x0f90 . #x0f95)
   #x0f97
   (#x0f99 . #x0fad)
   (#x0fb1 . #x0fb7)
   #x0fb9
   (#x20d0 . #x20dc)
   #x20e1
   #x3005
   (#x302a . #x302f)
   (#x3031 . #x3035)
   #x3099
   #x309a
   (#x309d . #x309e)
   (#x30fc . #x30fe)))

;;; Auto-generated section.

;; The rest of the file was auto-generated by doing M-x xsdre-gen-categories
;; on UnicodeData-3.1.0.txt available from
;; http://www.unicode.org/Public/3.1-Update/UnicodeData-3.1.0.txt

(xsdre-def-primitive-category 'Lu
			      '((65 . 90)
				(192 . 214)
				(216 . 222)
				256 258 260 262 264 266 268 270 272 274 276
				278 280 282 284 286 288 290 292 294 296 298
				300 302 304 306 308 310 313 315 317 319 321
				323 325 327 330 332 334 336 338 340 342 344
				346 348 350 352 354 356 358 360 362 364 366
				368 370 372 374
				(376 . 377)
				379 381
				(385 . 386)
				388
				(390 . 391)
				(393 . 395)
				(398 . 401)
				(403 . 404)
				(406 . 408)
				(412 . 413)
				(415 . 416)
				418 420
				(422 . 423)
				425 428
				(430 . 431)
				(433 . 435)
				437
				(439 . 440)
				444 452 455 458 461 463 465 467 469 471 473
				475 478 480 482 484 486 488 490 492 494 497
				500
				(502 . 504)
				506 508 510 512 514 516 518 520 522 524 526
				528 530 532 534 536 538 540 542 546 548 550
				552 554 556 558 560 562 902
				(904 . 906)
				908
				(910 . 911)
				(913 . 929)
				(931 . 939)
				(978 . 980)
				986 988 990 992 994 996 998 1000 1002 1004
				1006 1012
				(1024 . 1071)
				1120 1122 1124 1126 1128 1130 1132 1134 1136
				1138 1140 1142 1144 1146 1148 1150 1152 1164
				1166 1168 1170 1172 1174 1176 1178 1180 1182
				1184 1186 1188 1190 1192 1194 1196 1198 1200
				1202 1204 1206 1208 1210 1212 1214
				(1216 . 1217)
				1219 1223 1227 1232 1234 1236 1238 1240 1242
				1244 1246 1248 1250 1252 1254 1256 1258 1260
				1262 1264 1266 1268 1272
				(1329 . 1366)
				(4256 . 4293)
				7680 7682 7684 7686 7688 7690 7692 7694 7696
				7698 7700 7702 7704 7706 7708 7710 7712 7714
				7716 7718 7720 7722 7724 7726 7728 7730 7732
				7734 7736 7738 7740 7742 7744 7746 7748 7750
				7752 7754 7756 7758 7760 7762 7764 7766 7768
				7770 7772 7774 7776 7778 7780 7782 7784 7786
				7788 7790 7792 7794 7796 7798 7800 7802 7804
				7806 7808 7810 7812 7814 7816 7818 7820 7822
				7824 7826 7828 7840 7842 7844 7846 7848 7850
				7852 7854 7856 7858 7860 7862 7864 7866 7868
				7870 7872 7874 7876 7878 7880 7882 7884 7886
				7888 7890 7892 7894 7896 7898 7900 7902 7904
				7906 7908 7910 7912 7914 7916 7918 7920 7922
				7924 7926 7928
				(7944 . 7951)
				(7960 . 7965)
				(7976 . 7983)
				(7992 . 7999)
				(8008 . 8013)
				8025 8027 8029 8031
				(8040 . 8047)
				(8120 . 8123)
				(8136 . 8139)
				(8152 . 8155)
				(8168 . 8172)
				(8184 . 8187)
				8450 8455
				(8459 . 8461)
				(8464 . 8466)
				8469
				(8473 . 8477)
				8484 8486 8488
				(8490 . 8493)
				(8496 . 8497)
				8499
				(65313 . 65338)
				(66560 . 66597)
				(119808 . 119833)
				(119860 . 119885)
				(119912 . 119937)
				119964
				(119966 . 119967)
				119970
				(119973 . 119974)
				(119977 . 119980)
				(119982 . 119989)
				(120016 . 120041)
				(120068 . 120069)
				(120071 . 120074)
				(120077 . 120084)
				(120086 . 120092)
				(120120 . 120121)
				(120123 . 120126)
				(120128 . 120132)
				120134
				(120138 . 120144)
				(120172 . 120197)
				(120224 . 120249)
				(120276 . 120301)
				(120328 . 120353)
				(120380 . 120405)
				(120432 . 120457)
				(120488 . 120512)
				(120546 . 120570)
				(120604 . 120628)
				(120662 . 120686)
				(120720 . 120744)))
(xsdre-def-primitive-category 'Ll
			      '((97 . 122)
				170 181 186
				(223 . 246)
				(248 . 255)
				257 259 261 263 265 267 269 271 273 275 277
				279 281 283 285 287 289 291 293 295 297 299
				301 303 305 307 309
				(311 . 312)
				314 316 318 320 322 324 326
				(328 . 329)
				331 333 335 337 339 341 343 345 347 349 351
				353 355 357 359 361 363 365 367 369 371 373
				375 378 380
				(382 . 384)
				387 389 392
				(396 . 397)
				402 405
				(409 . 411)
				414 417 419 421 424
				(426 . 427)
				429 432 436 438
				(441 . 442)
				(445 . 447)
				454 457 460 462 464 466 468 470 472 474
				(476 . 477)
				479 481 483 485 487 489 491 493
				(495 . 496)
				499 501 505 507 509 511 513 515 517 519 521
				523 525 527 529 531 533 535 537 539 541 543
				547 549 551 553 555 557 559 561 563
				(592 . 685)
				912
				(940 . 974)
				(976 . 977)
				(981 . 983)
				987 989 991 993 995 997 999 1001 1003 1005

				(1007 . 1011)
				1013
				(1072 . 1119)
				1121 1123 1125 1127 1129 1131 1133 1135 1137
				1139 1141 1143 1145 1147 1149 1151 1153 1165
				1167 1169 1171 1173 1175 1177 1179 1181 1183
				1185 1187 1189 1191 1193 1195 1197 1199 1201
				1203 1205 1207 1209 1211 1213 1215 1218 1220
				1224 1228 1233 1235 1237 1239 1241 1243 1245
				1247 1249 1251 1253 1255 1257 1259 1261 1263
				1265 1267 1269 1273
				(1377 . 1415)
				7681 7683 7685 7687 7689 7691 7693 7695 7697
				7699 7701 7703 7705 7707 7709 7711 7713 7715
				7717 7719 7721 7723 7725 7727 7729 7731 7733
				7735 7737 7739 7741 7743 7745 7747 7749 7751
				7753 7755 7757 7759 7761 7763 7765 7767 7769
				7771 7773 7775 7777 7779 7781 7783 7785 7787
				7789 7791 7793 7795 7797 7799 7801 7803 7805
				7807 7809 7811 7813 7815 7817 7819 7821 7823
				7825 7827
				(7829 . 7835)
				7841 7843 7845 7847 7849 7851 7853 7855 7857
				7859 7861 7863 7865 7867 7869 7871 7873 7875
				7877 7879 7881 7883 7885 7887 7889 7891 7893
				7895 7897 7899 7901 7903 7905 7907 7909 7911
				7913 7915 7917 7919 7921 7923 7925 7927 7929

				(7936 . 7943)
				(7952 . 7957)
				(7968 . 7975)
				(7984 . 7991)
				(8000 . 8005)
				(8016 . 8023)
				(8032 . 8039)
				(8048 . 8061)
				(8064 . 8071)
				(8080 . 8087)
				(8096 . 8103)
				(8112 . 8116)
				(8118 . 8119)
				8126
				(8130 . 8132)
				(8134 . 8135)
				(8144 . 8147)
				(8150 . 8151)
				(8160 . 8167)
				(8178 . 8180)
				(8182 . 8183)
				8319 8458
				(8462 . 8463)
				8467 8495 8500 8505
				(64256 . 64262)
				(64275 . 64279)
				(65345 . 65370)
				(66600 . 66637)
				(119834 . 119859)
				(119886 . 119892)
				(119894 . 119911)
				(119938 . 119963)
				(119990 . 119993)
				119995
				(119997 . 120000)
				(120002 . 120003)
				(120005 . 120015)
				(120042 . 120067)
				(120094 . 120119)
				(120146 . 120171)
				(120198 . 120223)
				(120250 . 120275)
				(120302 . 120327)
				(120354 . 120379)
				(120406 . 120431)
				(120458 . 120483)
				(120514 . 120538)
				(120540 . 120545)
				(120572 . 120596)
				(120598 . 120603)
				(120630 . 120654)
				(120656 . 120661)
				(120688 . 120712)
				(120714 . 120719)
				(120746 . 120770)
				(120772 . 120777)))
(xsdre-def-primitive-category 'Lt
			      '(453 456 459 498
				    (8072 . 8079)
				    (8088 . 8095)
				    (8104 . 8111)
				    8124 8140 8188))
(xsdre-def-primitive-category 'Lm
			      '((688 . 696)
				(699 . 705)
				(720 . 721)
				(736 . 740)
				750 890 1369 1600
				(1765 . 1766)
				3654 3782 6211 12293
				(12337 . 12341)
				(12445 . 12446)
				(12540 . 12542)
				65392
				(65438 . 65439)))
(xsdre-def-primitive-category 'Lo
			      '(443
				(448 . 451)
				(1488 . 1514)
				(1520 . 1522)
				(1569 . 1594)
				(1601 . 1610)
				(1649 . 1747)
				1749
				(1786 . 1788)
				1808
				(1810 . 1836)
				(1920 . 1957)
				(2309 . 2361)
				2365 2384
				(2392 . 2401)
				(2437 . 2444)
				(2447 . 2448)
				(2451 . 2472)
				(2474 . 2480)
				2482
				(2486 . 2489)
				(2524 . 2525)
				(2527 . 2529)
				(2544 . 2545)
				(2565 . 2570)
				(2575 . 2576)
				(2579 . 2600)
				(2602 . 2608)
				(2610 . 2611)
				(2613 . 2614)
				(2616 . 2617)
				(2649 . 2652)
				2654
				(2674 . 2676)
				(2693 . 2699)
				2701
				(2703 . 2705)
				(2707 . 2728)
				(2730 . 2736)
				(2738 . 2739)
				(2741 . 2745)
				2749 2768 2784
				(2821 . 2828)
				(2831 . 2832)
				(2835 . 2856)
				(2858 . 2864)
				(2866 . 2867)
				(2870 . 2873)
				2877
				(2908 . 2909)
				(2911 . 2913)
				(2949 . 2954)
				(2958 . 2960)
				(2962 . 2965)
				(2969 . 2970)
				2972
				(2974 . 2975)
				(2979 . 2980)
				(2984 . 2986)
				(2990 . 2997)
				(2999 . 3001)
				(3077 . 3084)
				(3086 . 3088)
				(3090 . 3112)
				(3114 . 3123)
				(3125 . 3129)
				(3168 . 3169)
				(3205 . 3212)
				(3214 . 3216)
				(3218 . 3240)
				(3242 . 3251)
				(3253 . 3257)
				3294
				(3296 . 3297)
				(3333 . 3340)
				(3342 . 3344)
				(3346 . 3368)
				(3370 . 3385)
				(3424 . 3425)
				(3461 . 3478)
				(3482 . 3505)
				(3507 . 3515)
				3517
				(3520 . 3526)
				(3585 . 3632)
				(3634 . 3635)
				(3648 . 3653)
				(3713 . 3714)
				3716
				(3719 . 3720)
				3722 3725
				(3732 . 3735)
				(3737 . 3743)
				(3745 . 3747)
				3749 3751
				(3754 . 3755)
				(3757 . 3760)
				(3762 . 3763)
				3773
				(3776 . 3780)
				(3804 . 3805)
				3840
				(3904 . 3911)
				(3913 . 3946)
				(3976 . 3979)
				(4096 . 4129)
				(4131 . 4135)
				(4137 . 4138)
				(4176 . 4181)
				(4304 . 4342)
				(4352 . 4441)
				(4447 . 4514)
				(4520 . 4601)
				(4608 . 4614)
				(4616 . 4678)
				4680
				(4682 . 4685)
				(4688 . 4694)
				4696
				(4698 . 4701)
				(4704 . 4742)
				4744
				(4746 . 4749)
				(4752 . 4782)
				4784
				(4786 . 4789)
				(4792 . 4798)
				4800
				(4802 . 4805)
				(4808 . 4814)
				(4816 . 4822)
				(4824 . 4846)
				(4848 . 4878)
				4880
				(4882 . 4885)
				(4888 . 4894)
				(4896 . 4934)
				(4936 . 4954)
				(5024 . 5108)
				(5121 . 5740)
				(5743 . 5750)
				(5761 . 5786)
				(5792 . 5866)
				(6016 . 6067)
				(6176 . 6210)
				(6212 . 6263)
				(6272 . 6312)
				(8501 . 8504)
				12294
				(12353 . 12436)
				(12449 . 12538)
				(12549 . 12588)
				(12593 . 12686)
				(12704 . 12727)
				(13312 . 19893)
				(19968 . 40869)
				(40960 . 42124)
				(44032 . 55203)
				(63744 . 64045)
				64285
				(64287 . 64296)
				(64298 . 64310)
				(64312 . 64316)
				64318
				(64320 . 64321)
				(64323 . 64324)
				(64326 . 64433)
				(64467 . 64829)
				(64848 . 64911)
				(64914 . 64967)
				(65008 . 65019)
				(65136 . 65138)
				65140
				(65142 . 65276)
				(65382 . 65391)
				(65393 . 65437)
				(65440 . 65470)
				(65474 . 65479)
				(65482 . 65487)
				(65490 . 65495)
				(65498 . 65500)
				(66304 . 66334)
				(66352 . 66377)
				(131072 . 173782)
				(194560 . 195101)))
(xsdre-def-primitive-category 'Mn
			      '((768 . 846)
				(864 . 866)
				(1155 . 1158)
				(1425 . 1441)
				(1443 . 1465)
				(1467 . 1469)
				1471
				(1473 . 1474)
				1476
				(1611 . 1621)
				1648
				(1750 . 1756)
				(1759 . 1764)
				(1767 . 1768)
				(1770 . 1773)
				1809
				(1840 . 1866)
				(1958 . 1968)
				(2305 . 2306)
				2364
				(2369 . 2376)
				2381
				(2385 . 2388)
				(2402 . 2403)
				2433 2492
				(2497 . 2500)
				2509
				(2530 . 2531)
				2562 2620
				(2625 . 2626)
				(2631 . 2632)
				(2635 . 2637)
				(2672 . 2673)
				(2689 . 2690)
				2748
				(2753 . 2757)
				(2759 . 2760)
				2765 2817 2876 2879
				(2881 . 2883)
				2893 2902 2946 3008 3021
				(3134 . 3136)
				(3142 . 3144)
				(3146 . 3149)
				(3157 . 3158)
				3263 3270
				(3276 . 3277)
				(3393 . 3395)
				3405 3530
				(3538 . 3540)
				3542 3633
				(3636 . 3642)
				(3655 . 3662)
				3761
				(3764 . 3769)
				(3771 . 3772)
				(3784 . 3789)
				(3864 . 3865)
				3893 3895 3897
				(3953 . 3966)
				(3968 . 3972)
				(3974 . 3975)
				(3984 . 3991)
				(3993 . 4028)
				4038
				(4141 . 4144)
				4146
				(4150 . 4151)
				4153
				(4184 . 4185)
				(6071 . 6077)
				6086
				(6089 . 6099)
				6313
				(8400 . 8412)
				8417
				(12330 . 12335)
				(12441 . 12442)
				64286
				(65056 . 65059)
				(119143 . 119145)
				(119163 . 119170)
				(119173 . 119179)
				(119210 . 119213)))
(xsdre-def-primitive-category 'Mc
			      '(2307
				(2366 . 2368)
				(2377 . 2380)
				(2434 . 2435)
				(2494 . 2496)
				(2503 . 2504)
				(2507 . 2508)
				2519
				(2622 . 2624)
				2691
				(2750 . 2752)
				2761
				(2763 . 2764)
				(2818 . 2819)
				2878 2880
				(2887 . 2888)
				(2891 . 2892)
				2903 2947
				(3006 . 3007)
				(3009 . 3010)
				(3014 . 3016)
				(3018 . 3020)
				3031
				(3073 . 3075)
				(3137 . 3140)
				(3202 . 3203)
				3262
				(3264 . 3268)
				(3271 . 3272)
				(3274 . 3275)
				(3285 . 3286)
				(3330 . 3331)
				(3390 . 3392)
				(3398 . 3400)
				(3402 . 3404)
				3415
				(3458 . 3459)
				(3535 . 3537)
				(3544 . 3551)
				(3570 . 3571)
				(3902 . 3903)
				3967 4140 4145 4152
				(4182 . 4183)
				(6068 . 6070)
				(6078 . 6085)
				(6087 . 6088)
				(119141 . 119142)
				(119149 . 119154)))
(xsdre-def-primitive-category 'Me
			      '((1160 . 1161)
				(1757 . 1758)
				(8413 . 8416)
				(8418 . 8419)))
(xsdre-def-primitive-category 'Nd
			      '((48 . 57)
				(1632 . 1641)
				(1776 . 1785)
				(2406 . 2415)
				(2534 . 2543)
				(2662 . 2671)
				(2790 . 2799)
				(2918 . 2927)
				(3047 . 3055)
				(3174 . 3183)
				(3302 . 3311)
				(3430 . 3439)
				(3664 . 3673)
				(3792 . 3801)
				(3872 . 3881)
				(4160 . 4169)
				(4969 . 4977)
				(6112 . 6121)
				(6160 . 6169)
				(65296 . 65305)
				(120782 . 120831)))
(xsdre-def-primitive-category 'Nl
			      '((5870 . 5872)
				(8544 . 8579)
				12295
				(12321 . 12329)
				(12344 . 12346)
				66378))
(xsdre-def-primitive-category 'No
			      '((178 . 179)
				185
				(188 . 190)
				(2548 . 2553)
				(3056 . 3058)
				(3882 . 3891)
				(4978 . 4988)
				8304
				(8308 . 8313)
				(8320 . 8329)
				(8531 . 8543)
				(9312 . 9371)
				9450
				(10102 . 10131)
				(12690 . 12693)
				(12832 . 12841)
				(12928 . 12937)
				(66336 . 66339)))
(xsdre-def-primitive-category 'Pc
			      '(95
				(8255 . 8256)
				12539
				(65075 . 65076)
				(65101 . 65103)
				65343 65381))
(xsdre-def-primitive-category 'Pd
			      '(45 173 1418 6150
				   (8208 . 8213)
				   12316 12336
				   (65073 . 65074)
				   65112 65123 65293))
(xsdre-def-primitive-category 'Ps
			      '(40 91 123 3898 3900 5787 8218 8222 8261 8317
				   8333 9001 12296 12298 12300 12302 12304
				   12308 12310 12312 12314 12317 64830 65077
				   65079 65081 65083 65085 65087 65089 65091
				   65113 65115 65117 65288 65339 65371 65378))
(xsdre-def-primitive-category 'Pe
			      '(41 93 125 3899 3901 5788 8262 8318 8334 9002
				   12297 12299 12301 12303 12305 12309 12311
				   12313 12315
				   (12318 . 12319)
				   64831 65078 65080 65082 65084 65086 65088
				   65090 65092 65114 65116 65118 65289 65341
				   65373 65379))
(xsdre-def-primitive-category 'Pi
			      '(171 8216
				    (8219 . 8220)
				    8223 8249))
(xsdre-def-primitive-category 'Pf
			      '(187 8217 8221 8250))
(xsdre-def-primitive-category 'Po
			      '((33 . 35)
				(37 . 39)
				42 44
				(46 . 47)
				(58 . 59)
				(63 . 64)
				92 161 183 191 894 903
				(1370 . 1375)
				1417 1470 1472 1475
				(1523 . 1524)
				1548 1563 1567
				(1642 . 1645)
				1748
				(1792 . 1805)
				(2404 . 2405)
				2416 3572 3663
				(3674 . 3675)
				(3844 . 3858)
				3973
				(4170 . 4175)
				4347
				(4961 . 4968)
				(5741 . 5742)
				(5867 . 5869)
				(6100 . 6106)
				6108
				(6144 . 6149)
				(6151 . 6154)
				(8214 . 8215)
				(8224 . 8231)
				(8240 . 8248)
				(8251 . 8254)
				(8257 . 8259)
				(8264 . 8269)
				(12289 . 12291)
				65072
				(65097 . 65100)
				(65104 . 65106)
				(65108 . 65111)
				(65119 . 65121)
				65128
				(65130 . 65131)
				(65281 . 65283)
				(65285 . 65287)
				65290 65292
				(65294 . 65295)
				(65306 . 65307)
				(65311 . 65312)
				65340 65377 65380))
(xsdre-def-primitive-category 'Zs
			      '(32 160 5760
				   (8192 . 8203)
				   8239 12288))
(xsdre-def-primitive-category 'Zl
			      '(8232))
(xsdre-def-primitive-category 'Zp
			      '(8233))
(xsdre-def-primitive-category 'Sm
			      '(43
				(60 . 62)
				124 126 172 177 215 247 8260
				(8314 . 8316)
				(8330 . 8332)
				(8592 . 8596)
				(8602 . 8603)
				8608 8611 8614 8622
				(8654 . 8655)
				8658 8660
				(8704 . 8945)
				(8968 . 8971)
				(8992 . 8993)
				9655 9665 9839 64297 65122
				(65124 . 65126)
				65291
				(65308 . 65310)
				65372 65374 65506
				(65513 . 65516)
				120513 120539 120571 120597 120629 120655
				120687 120713 120745 120771))
(xsdre-def-primitive-category 'Sc
			      '(36
				(162 . 165)
				(2546 . 2547)
				3647 6107
				(8352 . 8367)
				65129 65284
				(65504 . 65505)
				(65509 . 65510)))
(xsdre-def-primitive-category 'Sk
			      '(94 96 168 175 180 184
				   (697 . 698)
				   (706 . 719)
				   (722 . 735)
				   (741 . 749)
				   (884 . 885)
				   (900 . 901)
				   8125
				   (8127 . 8129)
				   (8141 . 8143)
				   (8157 . 8159)
				   (8173 . 8175)
				   (8189 . 8190)
				   (12443 . 12444)
				   65342 65344 65507))
(xsdre-def-primitive-category 'So
			      '((166 . 167)
				169 174 176 182 1154 1769
				(1789 . 1790)
				2554 2928
				(3841 . 3843)
				(3859 . 3863)
				(3866 . 3871)
				3892 3894 3896
				(4030 . 4037)
				(4039 . 4044)
				4047
				(8448 . 8449)
				(8451 . 8454)
				(8456 . 8457)
				8468
				(8470 . 8472)
				(8478 . 8483)
				8485 8487 8489 8494 8498 8506
				(8597 . 8601)
				(8604 . 8607)
				(8609 . 8610)
				(8612 . 8613)
				(8615 . 8621)
				(8623 . 8653)
				(8656 . 8657)
				8659
				(8661 . 8691)
				(8960 . 8967)
				(8972 . 8991)
				(8994 . 9000)
				(9003 . 9083)
				(9085 . 9114)
				(9216 . 9254)
				(9280 . 9290)
				(9372 . 9449)
				(9472 . 9621)
				(9632 . 9654)
				(9656 . 9664)
				(9666 . 9719)
				(9728 . 9747)
				(9753 . 9838)
				(9840 . 9841)
				(9985 . 9988)
				(9990 . 9993)
				(9996 . 10023)
				(10025 . 10059)
				10061
				(10063 . 10066)
				10070
				(10072 . 10078)
				(10081 . 10087)
				10132
				(10136 . 10159)
				(10161 . 10174)
				(10240 . 10495)
				(11904 . 11929)
				(11931 . 12019)
				(12032 . 12245)
				(12272 . 12283)
				12292
				(12306 . 12307)
				12320
				(12342 . 12343)
				(12350 . 12351)
				(12688 . 12689)
				(12694 . 12703)
				(12800 . 12828)
				(12842 . 12867)
				(12896 . 12923)
				12927
				(12938 . 12976)
				(12992 . 13003)
				(13008 . 13054)
				(13056 . 13174)
				(13179 . 13277)
				(13280 . 13310)
				(42128 . 42145)
				(42148 . 42163)
				(42165 . 42176)
				(42178 . 42180)
				42182 65508 65512
				(65517 . 65518)
				(65532 . 65533)
				(118784 . 119029)
				(119040 . 119078)
				(119082 . 119140)
				(119146 . 119148)
				(119171 . 119172)
				(119180 . 119209)
				(119214 . 119261)))
(xsdre-def-primitive-category 'Cc
			      '((0 . 31)
				(127 . 159)))
(xsdre-def-primitive-category 'Cf
			      '(1807
				(6155 . 6158)
				(8204 . 8207)
				(8234 . 8238)
				(8298 . 8303)
				65279
				(65529 . 65531)
				(119155 . 119162)
				917505
				(917536 . 917631)))
(xsdre-def-primitive-category 'Co
			      '((57344 . 63743)
				(983040 . 1048573)
				(1048576 . 1114109)))

(provide 'xsd-regexp)

;;; xsd-regexp.el ends here
