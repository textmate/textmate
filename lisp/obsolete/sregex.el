;;; sregex.el --- symbolic regular expressions

;; Copyright (C) 1997-1998, 2000-2012 Free Software Foundation, Inc.

;; Author: Bob Glickstein <bobg+sregex@zanshin.com>
;; Maintainer: Bob Glickstein <bobg+sregex@zanshin.com>
;; Keywords: extensions
;; Obsolete-since: 24.1

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

;; This package allows you to write regular expressions using a
;; totally new, Lisp-like syntax.

;; A "symbolic regular expression" (sregex for short) is a Lisp form
;; that, when evaluated, produces the string form of the specified
;; regular expression.  Here's a simple example:

;;   (sregexq (or "Bob" "Robert"))  =>  "Bob\\|Robert"

;; As you can see, an sregex is specified by placing one or more
;; special clauses in a call to `sregexq'.  The clause in this case is
;; the `or' of two strings (not to be confused with the Lisp function
;; `or').  The list of allowable clauses appears below.

;; With sregex, it is never necessary to "escape" magic characters
;; that are meant to be taken literally; that happens automatically.
;; For example:

;;   (sregexq "M*A*S*H")  =>  "M\\*A\\*S\\*H"

;; It is also unnecessary to "group" parts of the expression together
;; to overcome operator precedence; that also happens automatically.
;; For example:

;;   (sregexq (opt (or "Bob" "Robert")))  =>  "\\(?:Bob\\|Robert\\)?"

;; It *is* possible to group parts of the expression in order to refer
;; to them with numbered backreferences:

;;   (sregexq (group (or "Go" "Run"))
;;            ", Spot, "
;;            (backref 1))             =>  "\\(Go\\|Run\\), Spot, \\1"

;; `sregexq' is a macro.  Each time it is used, it constructs a simple
;; Lisp expression that then invokes a moderately complex engine to
;; interpret the sregex and render the string form.  Because of this,
;; I don't recommend sprinkling calls to `sregexq' throughout your
;; code, the way one normally does with string regexes (which are
;; cheap to evaluate).  Instead, it's wiser to precompute the regexes
;; you need wherever possible instead of repeatedly constructing the
;; same ones over and over.  Example:

;;    (let ((field-regex (sregexq (opt "resent-")
;;                                (or "to" "cc" "bcc"))))
;;      ...
;;      (while ...
;;        ...
;;        (re-search-forward field-regex ...)
;;        ...))

;; The arguments to `sregexq' are automatically quoted, but the
;; flipside of this is that it is not straightforward to include
;; computed (i.e., non-constant) values in `sregexq' expressions.  So
;; `sregex' is a function that is like `sregexq' but which does not
;; automatically quote its values.  Literal sregex clauses must be
;; explicitly quoted like so:

;;   (sregex '(or "Bob" "Robert"))  =>  "Bob\\|Robert"

;; but computed clauses can be included easily, allowing for the reuse
;; of common clauses:

;;  (let ((dotstar '(0+ any))
;;        (whitespace '(1+ (syntax ?-)))
;;        (digits '(1+ (char (?0 . ?9)))))
;;    (sregex 'bol dotstar ":" whitespace digits))  =>  "^.*:\\s-+[0-9]+"

;; To use this package in a Lisp program, simply (require 'sregex).

;; Here are the clauses allowed in an `sregex' or `sregexq'
;; expression:

;; - a string
;;   This stands for the literal string.  If it contains
;;   metacharacters, they will be escaped in the resulting regex
;;   (using `regexp-quote').

;; - the symbol `any'
;;   This stands for ".", a regex matching any character except
;;   newline.

;; - the symbol `bol'
;;   Stands for "^", matching the empty string at the beginning of a line

;; - the symbol `eol'
;;   Stands for "$", matching the empty string at the end of a line

;; - (group CLAUSE ...)
;;   Groups the given CLAUSEs using "\\(" and "\\)".

;; - (sequence CLAUSE ...)

;;   Groups the given CLAUSEs; may or may not use "\\(?:" and "\\)".
;;   Clauses grouped by `sequence' do not count for purposes of
;;   numbering backreferences.  Use `sequence' in situations like
;;   this:

;;     (sregexq (or "dog" "cat"
;;                  (sequence (opt "sea ") "monkey")))
;;                                  =>  "dog\\|cat\\|\\(?:sea \\)?monkey"

;;   where a single `or' alternate needs to contain multiple
;;   subclauses.

;; - (backref N)
;;   Matches the same string previously matched by the Nth "group" in
;;   the same sregex.  N is a positive integer.

;; - (or CLAUSE ...)
;;   Matches any one of the CLAUSEs by separating them with "\\|".

;; - (0+ CLAUSE ...)
;;   Concatenates the given CLAUSEs and matches zero or more
;;   occurrences by appending "*".

;; - (1+ CLAUSE ...)
;;   Concatenates the given CLAUSEs and matches one or more
;;   occurrences by appending "+".

;; - (opt CLAUSE ...)
;;   Concatenates the given CLAUSEs and matches zero or one occurrence
;;   by appending "?".

;; - (repeat MIN MAX CLAUSE ...)
;;   Concatenates the given CLAUSEs and constructs a regex matching at
;;   least MIN occurrences and at most MAX occurrences.  MIN must be a
;;   non-negative integer.  MAX must be a non-negative integer greater
;;   than or equal to MIN; or MAX can be nil to mean "infinity."

;; - (char CHAR-CLAUSE ...)
;;   Creates a "character class" matching one character from the given
;;   set.  See below for how to construct a CHAR-CLAUSE.

;; - (not-char CHAR-CLAUSE ...)
;;   Creates a "character class" matching any one character not in the
;;   given set.  See below for how to construct a CHAR-CLAUSE.

;; - the symbol `bot'
;;   Stands for "\\`", matching the empty string at the beginning of
;;   text (beginning of a string or of a buffer).

;; - the symbol `eot'
;;   Stands for "\\'", matching the empty string at the end of text.

;; - the symbol `point'
;;   Stands for "\\=", matching the empty string at point.

;; - the symbol `word-boundary'
;;   Stands for "\\b", matching the empty string at the beginning or
;;   end of a word.

;; - the symbol `not-word-boundary'
;;   Stands for "\\B", matching the empty string not at the beginning
;;   or end of a word.

;; - the symbol `bow'
;;   Stands for "\\<", matching the empty string at the beginning of a
;;   word.

;; - the symbol `eow'
;;   Stands for "\\>", matching the empty string at the end of a word.

;; - the symbol `wordchar'
;;   Stands for the regex "\\w", matching a word-constituent character
;;   (as determined by the current syntax table)

;; - the symbol `not-wordchar'
;;   Stands for the regex "\\W", matching a non-word-constituent
;;   character.

;; - (syntax CODE)
;;   Stands for the regex "\\sCODE", where CODE is a syntax table code
;;   (a single character).  Matches any character with the requested
;;   syntax.

;; - (not-syntax CODE)
;;   Stands for the regex "\\SCODE", where CODE is a syntax table code
;;   (a single character).  Matches any character without the
;;   requested syntax.

;; - (regex REGEX)
;;   This is a "trapdoor" for including ordinary regular expression
;;   strings in the result.  Some regular expressions are clearer when
;;   written the old way: "[a-z]" vs. (sregexq (char (?a . ?z))), for
;;   instance.  However, see the note under "Bugs," below.

;; Each CHAR-CLAUSE that is passed to (char ...) and (not-char ...)
;; has one of the following forms:

;; - a character
;;   Adds that character to the set.

;; - a string
;;   Adds all the characters in the string to the set.

;; - A pair (MIN . MAX)
;;   Where MIN and MAX are characters, adds the range of characters
;;   from MIN through MAX to the set.

;;; To do:

;; An earlier version of this package could optionally translate the
;; symbolic regex into other languages' syntaxes, e.g. Perl.  For
;; instance, with Perl syntax selected, (sregexq (or "ab" "cd")) would
;; yield "ab|cd" instead of "ab\\|cd".  It might be useful to restore
;; such a facility.

;; - handle multibyte chars in sregex--char-aux
;; - add support for character classes ([:blank:], ...)
;; - add support for non-greedy operators *? and +?
;; - bug: (sregexq (opt (opt ?a))) returns "a??" which is a non-greedy "a?"

;;; Bugs:

;;; Code:

(eval-when-compile (require 'cl))

;; Compatibility code for when we didn't have shy-groups
(defvar sregex--current-sregex nil)
(defun sregex-info () nil)
(defmacro sregex-save-match-data (&rest forms) (cons 'save-match-data forms))
(defun sregex-replace-match (r &optional f l str subexp x)
  (replace-match r f l str subexp))
(defun sregex-match-string (c &optional i x) (match-string c i))
(defun sregex-match-string-no-properties (count &optional in-string sregex)
  (match-string-no-properties count in-string))
(defun sregex-match-beginning (count &optional sregex) (match-beginning count))
(defun sregex-match-end (count &optional sregex) (match-end count))
(defun sregex-match-data (&optional sregex) (match-data))
(defun sregex-backref-num (n &optional sregex) n)


(defun sregex (&rest exps)
  "Symbolic regular expression interpreter.
This is exactly like `sregexq' (q.v.) except that it evaluates all its
arguments, so literal sregex clauses must be quoted.  For example:

  (sregex '(or \"Bob\" \"Robert\"))  =>  \"Bob\\\\|Robert\"

An argument-evaluating sregex interpreter lets you reuse sregex
subexpressions:

  (let ((dotstar '(0+ any))
        (whitespace '(1+ (syntax ?-)))
        (digits '(1+ (char (?0 . ?9)))))
    (sregex 'bol dotstar \":\" whitespace digits))  =>  \"^.*:\\\\s-+[0-9]+\""
  (sregex--sequence exps nil))

(defmacro sregexq (&rest exps)
  "Symbolic regular expression interpreter.
This macro allows you to specify a regular expression (regexp) in
symbolic form, and converts it into the string form required by Emacs's
regex functions such as `re-search-forward' and `looking-at'.  Here is
a simple example:

  (sregexq (or \"Bob\" \"Robert\"))  =>  \"Bob\\\\|Robert\"

As you can see, an sregex is specified by placing one or more special
clauses in a call to `sregexq'.  The clause in this case is the `or'
of two strings (not to be confused with the Lisp function `or').  The
list of allowable clauses appears below.

With `sregex', it is never necessary to \"escape\" magic characters
that are meant to be taken literally; that happens automatically.
For example:

  (sregexq \"M*A*S*H\")  =>  \"M\\\\*A\\\\*S\\\\*H\"

It is also unnecessary to \"group\" parts of the expression together
to overcome operator precedence; that also happens automatically.
For example:

  (sregexq (opt (or \"Bob\" \"Robert\")))  =>  \"\\\\(Bob\\\\|Robert\\\\)?\"

It *is* possible to group parts of the expression in order to refer
to them with numbered backreferences:

  (sregexq (group (or \"Go\" \"Run\"))
           \", Spot, \"
           (backref 1))             =>  \"\\\\(Go\\\\|Run\\\\), Spot, \\\\1\"

If `sregexq' needs to introduce its own grouping parentheses, it will
automatically renumber your backreferences:

  (sregexq (opt \"resent-\")
           (group (or \"to\" \"cc\" \"bcc\"))
           \": \"
           (backref 1))  =>  \"\\\\(resent-\\\\)?\\\\(to\\\\|cc\\\\|bcc\\\\): \\\\2\"

`sregexq' is a macro.  Each time it is used, it constructs a simple
Lisp expression that then invokes a moderately complex engine to
interpret the sregex and render the string form.  Because of this, I
don't recommend sprinkling calls to `sregexq' throughout your code,
the way one normally does with string regexes (which are cheap to
evaluate).  Instead, it's wiser to precompute the regexes you need
wherever possible instead of repeatedly constructing the same ones
over and over.  Example:

   (let ((field-regex (sregexq (opt \"resent-\")
                               (or \"to\" \"cc\" \"bcc\"))))
     ...
     (while ...
       ...
       (re-search-forward field-regex ...)
       ...))

The arguments to `sregexq' are automatically quoted, but the
flipside of this is that it is not straightforward to include
computed (i.e., non-constant) values in `sregexq' expressions.  So
`sregex' is a function that is like `sregexq' but which does not
automatically quote its values.  Literal sregex clauses must be
explicitly quoted like so:

  (sregex '(or \"Bob\" \"Robert\"))  =>  \"Bob\\\\|Robert\"

but computed clauses can be included easily, allowing for the reuse
of common clauses:

  (let ((dotstar '(0+ any))
        (whitespace '(1+ (syntax ?-)))
        (digits '(1+ (char (?0 . ?9)))))
    (sregex 'bol dotstar \":\" whitespace digits))  =>  \"^.*:\\\\s-+[0-9]+\"

Here are the clauses allowed in an `sregex' or `sregexq' expression:

- a string
  This stands for the literal string.  If it contains
  metacharacters, they will be escaped in the resulting regex
  (using `regexp-quote').

- the symbol `any'
  This stands for \".\", a regex matching any character except
  newline.

- the symbol `bol'
  Stands for \"^\", matching the empty string at the beginning of a line

- the symbol `eol'
  Stands for \"$\", matching the empty string at the end of a line

- (group CLAUSE ...)
  Groups the given CLAUSEs using \"\\\\(\" and \"\\\\)\".

- (sequence CLAUSE ...)

  Groups the given CLAUSEs; may or may not use \"\\\\(\" and \"\\\\)\".
  Clauses grouped by `sequence' do not count for purposes of
  numbering backreferences.  Use `sequence' in situations like
  this:

    (sregexq (or \"dog\" \"cat\"
                 (sequence (opt \"sea \") \"monkey\")))
                                 =>  \"dog\\\\|cat\\\\|\\\\(?:sea \\\\)?monkey\"

  where a single `or' alternate needs to contain multiple
  subclauses.

- (backref N)
  Matches the same string previously matched by the Nth \"group\" in
  the same sregex.  N is a positive integer.

- (or CLAUSE ...)
  Matches any one of the CLAUSEs by separating them with \"\\\\|\".

- (0+ CLAUSE ...)
  Concatenates the given CLAUSEs and matches zero or more
  occurrences by appending \"*\".

- (1+ CLAUSE ...)
  Concatenates the given CLAUSEs and matches one or more
  occurrences by appending \"+\".

- (opt CLAUSE ...)
  Concatenates the given CLAUSEs and matches zero or one occurrence
  by appending \"?\".

- (repeat MIN MAX CLAUSE ...)
  Concatenates the given CLAUSEs and constructs a regex matching at
  least MIN occurrences and at most MAX occurrences.  MIN must be a
  non-negative integer.  MAX must be a non-negative integer greater
  than or equal to MIN; or MAX can be nil to mean \"infinity.\"

- (char CHAR-CLAUSE ...)
  Creates a \"character class\" matching one character from the given
  set.  See below for how to construct a CHAR-CLAUSE.

- (not-char CHAR-CLAUSE ...)
  Creates a \"character class\" matching any one character not in the
  given set.  See below for how to construct a CHAR-CLAUSE.

- the symbol `bot'
  Stands for \"\\\\`\", matching the empty string at the beginning of
  text (beginning of a string or of a buffer).

- the symbol `eot'
  Stands for \"\\\\'\", matching the empty string at the end of text.

- the symbol `point'
  Stands for \"\\\\=\\=\", matching the empty string at point.

- the symbol `word-boundary'
  Stands for \"\\\\b\", matching the empty string at the beginning or
  end of a word.

- the symbol `not-word-boundary'
  Stands for \"\\\\B\", matching the empty string not at the beginning
  or end of a word.

- the symbol `bow'
  Stands for \"\\\\=\\<\", matching the empty string at the beginning of a
  word.

- the symbol `eow'
  Stands for \"\\\\=\\>\", matching the empty string at the end of a word.

- the symbol `wordchar'
  Stands for the regex \"\\\\w\", matching a word-constituent character
  (as determined by the current syntax table)

- the symbol `not-wordchar'
  Stands for the regex \"\\\\W\", matching a non-word-constituent
  character.

- (syntax CODE)
  Stands for the regex \"\\\\sCODE\", where CODE is a syntax table code
  (a single character).  Matches any character with the requested
  syntax.

- (not-syntax CODE)
  Stands for the regex \"\\\\SCODE\", where CODE is a syntax table code
  (a single character).  Matches any character without the
  requested syntax.

- (regex REGEX)
  This is a \"trapdoor\" for including ordinary regular expression
  strings in the result.  Some regular expressions are clearer when
  written the old way: \"[a-z]\" vs. (sregexq (char (?a . ?z))), for
  instance.

Each CHAR-CLAUSE that is passed to (char ...) and (not-char ...)
has one of the following forms:

- a character
  Adds that character to the set.

- a string
  Adds all the characters in the string to the set.

- A pair (MIN . MAX)
  Where MIN and MAX are characters, adds the range of characters
  from MIN through MAX to the set."
  `(apply 'sregex ',exps))

(defun sregex--engine (exp combine)
  (cond
   ((stringp exp)
    (if (and combine
	     (eq combine 'suffix)
	     (/= (length exp) 1))
	(concat "\\(?:" (regexp-quote exp) "\\)")
      (regexp-quote exp)))
   ((symbolp exp)
    (ecase exp
      (any ".")
      (bol "^")
      (eol "$")
      (wordchar "\\w")
      (not-wordchar "\\W")
      (bot "\\`")
      (eot "\\'")
      (point "\\=")
      (word-boundary "\\b")
      (not-word-boundary "\\B")
      (bow "\\<")
      (eow "\\>")))
   ((consp exp)
    (funcall (intern (concat "sregex--"
			     (symbol-name (car exp))))
	     (cdr exp)
	     combine))
   (t (error "Invalid expression: %s" exp))))

(defun sregex--sequence (exps combine)
  (if (= (length exps) 1) (sregex--engine (car exps) combine)
    (let ((re (mapconcat
	       (lambda (e) (sregex--engine e 'concat))
	       exps "")))
      (if (eq combine 'suffix)
          (concat "\\(?:" re "\\)")
        re))))

(defun sregex--or (exps combine)
  (if (= (length exps) 1) (sregex--engine (car exps) combine)
    (let ((re (mapconcat
	       (lambda (e) (sregex--engine e 'or))
	       exps "\\|")))
      (if (not (eq combine 'or))
          (concat "\\(?:" re "\\)")
        re))))

(defun sregex--group (exps combine) (concat "\\(" (sregex--sequence exps nil) "\\)"))

(defun sregex--backref (exps combine) (concat "\\" (int-to-string (car exps))))
(defun sregex--opt (exps combine) (concat (sregex--sequence exps 'suffix) "?"))
(defun sregex--0+ (exps combine) (concat (sregex--sequence exps 'suffix) "*"))
(defun sregex--1+ (exps combine) (concat (sregex--sequence exps 'suffix) "+"))

(defun sregex--char (exps combine) (sregex--char-aux nil exps))
(defun sregex--not-char (exps combine) (sregex--char-aux t exps))

(defun sregex--syntax (exps combine) (format "\\s%c" (car exps)))
(defun sregex--not-syntax (exps combine) (format "\\S%c" (car exps)))

(defun sregex--regex (exps combine)
  (if combine (concat "\\(?:" (car exps) "\\)") (car exps)))

(defun sregex--repeat (exps combine)
  (let* ((min (or (pop exps) 0))
	 (minstr (number-to-string min))
	 (max (pop exps)))
    (concat (sregex--sequence exps 'suffix)
	    (concat "\\{" minstr ","
		    (when max (number-to-string max)) "\\}"))))

(defun sregex--char-range (start end)
  (let ((startc (char-to-string start))
	(endc (char-to-string end)))
    (cond
     ((> end (+ start 2)) (concat startc "-" endc))
     ((> end (+ start 1)) (concat startc (char-to-string (1+ start)) endc))
     ((> end start) (concat startc endc))
     (t startc))))

(defun sregex--char-aux (complement args)
  ;; regex-opt does the same, we should join effort.
  (let ((chars (make-bool-vector 256 nil))) ; Yeah, right!
    (dolist (arg args)
      (cond ((integerp arg) (aset chars arg t))
	    ((stringp arg) (mapc (lambda (c) (aset chars c t)) arg))
	    ((consp arg)
	     (let ((start (car arg))
		   (end (cdr arg)))
	       (when (> start end)
		 (let ((tmp start)) (setq start end) (setq end tmp)))
	       ;; now start <= end
	       (let ((i start))
		 (while (<= i end)
		   (aset chars i t)
		   (setq i (1+ i))))))))
    ;; now chars is a map of the characters in the class
    (let ((caret (aref chars ?^))
	  (dash (aref chars ?-))
	  (class (if (aref chars ?\]) "]" "")))
      (aset chars ?^ nil)
      (aset chars ?- nil)
      (aset chars ?\] nil)

      (let (start end)
	(dotimes (i 256)
	  (if (aref chars i)
	      (progn
		(unless start (setq start i))
		(setq end i)
		(aset chars i nil))
	    (when start
	      (setq class (concat class (sregex--char-range start end)))
	      (setq start nil))))
	(if start
	    (setq class (concat class (sregex--char-range start end)))))

      (if (> (length class) 0)
	  (setq class (concat class (if caret "^") (if dash "-")))
	(setq class (concat class (if dash "-") (if caret "^"))))
      (if (and (not complement) (= (length class) 1))
	  (regexp-quote class)
	(concat "[" (if complement "^") class "]")))))

(provide 'sregex)

;;; sregex.el ends here
