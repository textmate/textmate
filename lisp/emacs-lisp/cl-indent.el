;;; cl-indent.el --- enhanced lisp-indent mode

;; Copyright (C) 1987, 2000-2012 Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@eddie.mit.edu>
;; Created: July 1987
;; Maintainer: FSF
;; Keywords: lisp, tools
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

;; This package supplies a single entry point, common-lisp-indent-function,
;; which performs indentation in the preferred style for Common Lisp code.
;; To enable it:
;;
;; (setq lisp-indent-function 'common-lisp-indent-function)

;;; Code:

(eval-when-compile (require 'cl))

(defgroup lisp-indent nil
  "Indentation in Lisp."
  :group 'lisp)


(defcustom lisp-indent-maximum-backtracking 3
  "Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is 0, no backtracking will occur and forms such as `flet'
may not be correctly indented."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-tag-indentation 1
  "Indentation of tags relative to containing list.
This variable is used by the function `lisp-indent-tagbody'."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-tag-body-indentation 3
  "Indentation of non-tagged lines relative to containing list.
This variable is used by the function `lisp-indent-tagbody' to indent normal
lines (lines without tags).
The indentation is relative to the indentation of the parenthesis enclosing
the special form.  If the value is t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by `lisp-body-indent'."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-backquote-indentation t
  "Whether or not to indent backquoted lists as code.
If nil, indent backquoted lists as data, i.e., like quoted lists."
  :type 'boolean
  :group 'lisp-indent)


(defcustom lisp-loop-keyword-indentation 3
  "Indentation of loop keywords in extended loop forms."
  :type 'integer
  :group 'lisp-indent)


(defcustom lisp-loop-forms-indentation 5
  "Indentation of forms in extended loop forms."
  :type 'integer
  :group 'lisp-indent)


(defcustom lisp-simple-loop-indentation 3
  "Indentation of forms in simple loop forms."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-lambda-list-keyword-alignment nil
  "Whether to vertically align lambda-list keywords together.
If nil (the default), keyworded lambda-list parts are aligned
with the initial mandatory arguments, like this:

\(defun foo (arg1 arg2 &rest rest
            &key key1 key2)
  #|...|#)

If non-nil, alignment is done with the first keyword
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &rest rest
                      &key key1 key2)
  #|...|#)"
  :version "24.1"
  :type 'boolean
  :group 'lisp-indent)

(defcustom lisp-lambda-list-keyword-parameter-indentation 2
  "Indentation of lambda list keyword parameters.
See `lisp-lambda-list-keyword-parameter-alignment'
for more information."
  :version "24.1"
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-lambda-list-keyword-parameter-alignment nil
  "Whether to vertically align lambda-list keyword parameters together.
If nil (the default), the parameters are aligned
with their corresponding keyword, plus the value of
`lisp-lambda-list-keyword-parameter-indentation', like this:

\(defun foo (arg1 arg2 &key key1 key2
                        key3 key4)
  #|...|#)

If non-nil, alignment is done with the first parameter
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &key key1 key2
                            key3 key4)
  #|...|#)"
  :version "24.1"
  :type 'boolean
  :group 'lisp-indent)


(defvar lisp-indent-defun-method '(4 &lambda &body)
  "Defun-like indentation method.
This applies when the value of the `common-lisp-indent-function' property
is set to `defun'.")


(defun extended-loop-p (loop-start)
  "True if an extended loop form starts at LOOP-START."
  (condition-case ()
      (save-excursion
	(goto-char loop-start)
	(forward-char 1)
	(forward-sexp 2)
	(backward-sexp 1)
	(looking-at "\\sw"))
    (error t)))


(defun common-lisp-loop-part-indentation (indent-point state)
  "Compute the indentation of loop form constituents."
  (let* ((loop-indentation (save-excursion
			     (goto-char (elt state 1))
			     (current-column))))
    (goto-char indent-point)
    (beginning-of-line)
    (list
     (cond ((not (extended-loop-p (elt state 1)))
	    (+ loop-indentation lisp-simple-loop-indentation))
	   ((looking-at "^\\s-*\\(:?\\sw+\\|;\\)")
	    (+ loop-indentation lisp-loop-keyword-indentation))
	   (t
	    (+ loop-indentation lisp-loop-forms-indentation)))
     ;; Tell the caller that the next line needs recomputation, even
     ;; though it doesn't start a sexp.
     loop-indentation)))


;; Cf (info "(elisp)Specification List")
;;;###autoload
(defun common-lisp-indent-function (indent-point state)
  "Function to indent the arguments of a Lisp function call.
This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the
`parse-partial-sexp' state at that position.  Browse the
`lisp-indent' customize group for options affecting the behavior
of this function.

If the indentation point is in a call to a Lisp function, that
function's `common-lisp-indent-function' property specifies how
this function should indent it.  Possible values for this
property are:

* defun, meaning indent according to `lisp-indent-defun-method';
  i.e., like (4 &lambda &body), as explained below.

* any other symbol, meaning a function to call.  The function should
  take the arguments: PATH STATE INDENT-POINT SEXP-COLUMN NORMAL-INDENT.
  PATH is a list of integers describing the position of point in terms of
  list-structure with respect to the containing lists.  For example, in
  ((a b c (d foo) f) g), foo has a path of (0 3 1).  In other words,
  to reach foo take the 0th element of the outermost list, then
  the 3rd element of the next list, and finally the 1st element.
  STATE and INDENT-POINT are as in the arguments to
  `common-lisp-indent-function'.  SEXP-COLUMN is the column of
  the open parenthesis of the innermost containing list.
  NORMAL-INDENT is the column the indentation point was
  originally in.  This function should behave like `lisp-indent-259'.

* an integer N, meaning indent the first N arguments like
  function arguments, and any further arguments like a body.
  This is equivalent to (4 4 ... &body).

* a list.  The list element in position M specifies how to indent the Mth
  function argument.  If there are fewer elements than function arguments,
  the last list element applies to all remaining arguments.  The accepted
  list elements are:

  * nil, meaning the default indentation.

  * an integer, specifying an explicit indentation.

  * &lambda.  Indent the argument (which may be a list) by 4.

  * &rest.  When used, this must be the penultimate element.  The
    element after this one applies to all remaining arguments.

  * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
    all remaining elements by `lisp-body-indent'.

  * &whole.  This must be followed by nil, an integer, or a
    function symbol.  This indentation is applied to the
    associated argument, and as a base indent for all remaining
    arguments.  For example, an integer P means indent this
    argument by P, and all remaining arguments by P, plus the
    value specified by their associated list element.

  * a symbol.  A function to call, with the 6 arguments specified above.

  * a list, with elements as described above.  This applies when the
    associated function argument is itself a list.  Each element of the list
    specifies how to indent the associated argument.

For example, the function `case' has an indent property
\(4 &rest (&whole 2 &rest 1)), meaning:
  * indent the first argument by 4.
  * arguments after the first should be lists, and there may be any number
    of them.  The first list element has an offset of 2, all the rest
    have an offset of 2+1=3."
  (if (save-excursion (goto-char (elt state 1))
		      (looking-at "([Ll][Oo][Oo][Pp]"))
      (common-lisp-loop-part-indentation indent-point state)
    (common-lisp-indent-function-1 indent-point state)))


(defun common-lisp-indent-function-1 (indent-point state)
  (let ((normal-indent (current-column)))
    ;; Walk up list levels until we see something
    ;;  which does special things with subforms.
    (let ((depth 0)
          ;; Path describes the position of point in terms of
          ;;  list-structure with respect to containing lists.
          ;; `foo' has a path of (0 3 1) in `((a b c (d foo) f) g)'.
          (path ())
          ;; set non-nil when somebody works out the indentation to use
          calculated
	  ;; If non-nil, this is an indentation to use
	  ;; if nothing else specifies it more firmly.
	  tentative-calculated
	  (last-point indent-point)
          ;; the position of the open-paren of the innermost containing list
          (containing-form-start (elt state 1))
          ;; the column of the above
          sexp-column)
      ;; Move to start of innermost containing list
      (goto-char containing-form-start)
      (setq sexp-column (current-column))

      ;; Look over successively less-deep containing forms
      (while (and (not calculated)
                  (< depth lisp-indent-maximum-backtracking))
        (let ((containing-sexp (point)))
          (forward-char 1)
          (parse-partial-sexp (point) indent-point 1 t)
          ;; Move to the car of the relevant containing form
          (let (tem function method tentative-defun)
            (if (not (looking-at "\\sw\\|\\s_"))
                ;; This form doesn't seem to start with a symbol
                (setq function nil method nil)
              (setq tem (point))
              (forward-sexp 1)
              (setq function (downcase (buffer-substring-no-properties
                                        tem (point))))
              (goto-char tem)
              (setq tem (intern-soft function)
                    method (get tem 'common-lisp-indent-function))
              (cond ((and (null method)
                          (string-match ":[^:]+" function))
                     ;; The pleblisp package feature
                     (setq function (substring function
                                               (1+ (match-beginning 0)))
                           method (get (intern-soft function)
                                       'common-lisp-indent-function)))
                    ((and (null method))
                     ;; backwards compatibility
                     (setq method (get tem 'lisp-indent-function)))))
            (let ((n 0))
              ;; How far into the containing form is the current form?
              (if (< (point) indent-point)
                  (while (condition-case ()
                             (progn
                               (forward-sexp 1)
                               (if (>= (point) indent-point)
                                   nil
                                 (parse-partial-sexp (point)
                                                     indent-point 1 t)
                                 (setq n (1+ n))
                                 t))
                           (error nil))))
              (setq path (cons n path)))

            ;; backwards compatibility.
            (cond ((null function))
                  ((null method)
                   (when (null (cdr path))
		     ;; (package prefix was stripped off above)
		     (cond ((string-match "\\`def"
					  function)
			    (setq tentative-defun t))
			   ((string-match
                             (eval-when-compile
                              (concat "\\`\\("
                                      (regexp-opt '("with" "without" "do"))
                                      "\\)-"))
                             function)
			    (setq method '(&lambda &body))))))
                  ;; backwards compatibility.  Bletch.
                  ((eq method 'defun)
                   (setq method lisp-indent-defun-method)))

            (cond ((and (or (eq (char-after (1- containing-sexp)) ?\')
			    (and (not lisp-backquote-indentation)
				 (eq (char-after (1- containing-sexp)) ?\`)))
                        (not (eq (char-after (- containing-sexp 2)) ?\#)))
                   ;; No indentation for "'(...)" elements
                   (setq calculated (1+ sexp-column)))
                  ((or (eq (char-after (1- containing-sexp)) ?\,)
                       (and (eq (char-after (1- containing-sexp)) ?\@)
                            (eq (char-after (- containing-sexp 2)) ?\,)))
                   ;; ",(...)" or ",@(...)"
                   (setq calculated normal-indent))
                  ((eq (char-after (1- containing-sexp)) ?\#)
                   ;; "#(...)"
                   (setq calculated (1+ sexp-column)))
                  ((null method)
		   ;; If this looks like a call to a `def...' form,
		   ;; think about indenting it as one, but do it
		   ;; tentatively for cases like
		   ;; (flet ((defunp ()
		   ;;          nil)))
		   ;; Set both normal-indent and tentative-calculated.
		   ;; The latter ensures this value gets used
		   ;; if there are no relevant containing constructs.
		   ;; The former ensures this value gets used
		   ;; if there is a relevant containing construct
		   ;; but we are nested within the structure levels
		   ;; that it specifies indentation for.
		   (if tentative-defun
		       (setq tentative-calculated
			     (common-lisp-indent-call-method
			      function lisp-indent-defun-method
			      path state indent-point
			      sexp-column normal-indent)
			     normal-indent tentative-calculated)))
                  ((integerp method)
                   ;; convenient top-level hack.
                   ;;  (also compatible with lisp-indent-function)
                   ;; The number specifies how many `distinguished'
                   ;;  forms there are before the body starts
                   ;; Equivalent to (4 4 ... &body)
                   (setq calculated (cond ((cdr path)
                                           normal-indent)
                                          ((<= (car path) method)
                                           ;; `distinguished' form
                                           (list (+ sexp-column 4)
                                                 containing-form-start))
                                          ((= (car path) (1+ method))
                                           ;; first body form.
                                           (+ sexp-column lisp-body-indent))
                                          (t
                                           ;; other body form
                                           normal-indent))))
		  (t
		   (setq calculated
			 (common-lisp-indent-call-method
			  function method path state indent-point
			  sexp-column normal-indent)))))
          (goto-char containing-sexp)
          (setq last-point containing-sexp)
          (unless calculated
	    (condition-case ()
		(progn (backward-up-list 1)
		       (setq depth (1+ depth)))
	      (error (setq depth lisp-indent-maximum-backtracking))))))
      (or calculated tentative-calculated))))


(defun common-lisp-indent-call-method (function method path state indent-point
				       sexp-column normal-indent)
  (let ((lisp-indent-error-function function))
    (if (symbolp method)
	(funcall method
		 path state indent-point
		 sexp-column normal-indent)
      (lisp-indent-259 method path state indent-point
		       sexp-column normal-indent))))

;; Dynamically bound in common-lisp-indent-call-method.
(defvar lisp-indent-error-function)

(defun lisp-indent-report-bad-format (m)
  (error "%s has a badly-formed %s property: %s"
         ;; Love those free variable references!!
         lisp-indent-error-function 'common-lisp-indent-function m))


;; Lambda-list indentation is now done in LISP-INDENT-LAMBDA-LIST.
;; See also `lisp-lambda-list-keyword-alignment',
;; `lisp-lambda-list-keyword-parameter-alignment' and
;; `lisp-lambda-list-keyword-parameter-indentation' -- dvl

(defvar lisp-indent-lambda-list-keywords-regexp
  "&\\(\
optional\\|rest\\|key\\|allow-other-keys\\|aux\\|whole\\|body\\|environment\
\\)\\([ \t]\\|$\\)"
  "Regular expression matching lambda-list keywords.")

(defun lisp-indent-lambda-list
    (indent-point sexp-column containing-form-start)
  (let (limit)
    (cond ((save-excursion
	     (goto-char indent-point)
	     (beginning-of-line)
	     (skip-chars-forward " \t")
	     (setq limit (point))
	     (looking-at lisp-indent-lambda-list-keywords-regexp))
	   ;; We're facing a lambda-list keyword.
	   (if lisp-lambda-list-keyword-alignment
	       ;; Align to the first keyword if any, or to the beginning of
	       ;; the lambda-list.
	       (save-excursion
		 (goto-char containing-form-start)
		 (save-match-data
		   (if (re-search-forward
			lisp-indent-lambda-list-keywords-regexp
			limit t)
		       (progn
			 (goto-char (match-beginning 0))
			 (current-column))
		       (1+ sexp-column))))
	       ;; Align to the beginning of the lambda-list.
	       (1+ sexp-column)))
	  (t
	   ;; Otherwise, align to the first argument of the last lambda-list
	   ;; keyword, the keyword itself, or the beginning of the
	   ;; lambda-list.
	   (save-excursion
	     (goto-char indent-point)
	     (forward-line -1)
	     (end-of-line)
	     (save-match-data
	       (if (re-search-backward lisp-indent-lambda-list-keywords-regexp
				       containing-form-start t)
		   (let* ((keyword-posn
			   (progn
			     (goto-char (match-beginning 0))
			     (current-column)))
			  (indented-keyword-posn
			   (+ keyword-posn
			      lisp-lambda-list-keyword-parameter-indentation)))
		     (goto-char (match-end 0))
		     (skip-chars-forward " \t")
		     (if (eolp)
			 indented-keyword-posn
			 (if lisp-lambda-list-keyword-parameter-alignment
			     (current-column)
			     indented-keyword-posn)))
		   (1+ sexp-column))))))))

;; Blame the crufty control structure on dynamic scoping
;;  -- not on me!
(defun lisp-indent-259
    (method path state indent-point sexp-column normal-indent)
  (catch 'exit
    (let ((p path)
          (containing-form-start (elt state 1))
          n tem tail)
      ;; Isn't tail-recursion wonderful?
      (while p
        ;; This while loop is for destructuring.
        ;; p is set to (cdr p) each iteration.
        (if (not (consp method)) (lisp-indent-report-bad-format method))
        (setq n (1- (car p))
              p (cdr p)
              tail nil)
        (while n
          ;; This while loop is for advancing along a method
          ;; until the relevant (possibly &rest/&body) pattern
          ;; is reached.
          ;; n is set to (1- n) and method to (cdr method)
          ;; each iteration.
          (setq tem (car method))

          (or (eq tem 'nil)             ;default indentation
              (eq tem '&lambda)         ;lambda list
              (and (eq tem '&body) (null (cdr method)))
              (and (eq tem '&rest)
                   (consp (cdr method))
                   (null (cddr method)))
              (integerp tem)            ;explicit indentation specified
              (and (consp tem)          ;destructuring
                   (eq (car tem) '&whole)
                   (or (symbolp (cadr tem))
                       (integerp (cadr tem))))
              (and (symbolp tem)        ;a function to call to do the work.
                   (null (cdr method)))
              (lisp-indent-report-bad-format method))

          (cond ((and tail (not (consp tem)))
                 ;; indent tail of &rest in same way as first elt of rest
                 (throw 'exit normal-indent))
                ((eq tem '&body)
                 ;; &body means (&rest <lisp-body-indent>)
                 (throw 'exit
                   (if (and (= n 0)     ;first body form
                            (null p))   ;not in subforms
                       (+ sexp-column
                          lisp-body-indent)
                       normal-indent)))
                ((eq tem '&rest)
                 ;; this pattern holds for all remaining forms
                 (setq tail (> n 0)
                       n 0
                       method (cdr method)))
                ((> n 0)
                 ;; try next element of pattern
                 (setq n (1- n)
                       method (cdr method))
                 (if (< n 0)
                     ;; Too few elements in pattern.
                     (throw 'exit normal-indent)))
                ((eq tem 'nil)
		 (throw 'exit (if (consp normal-indent)
				  normal-indent
				(list normal-indent containing-form-start))))
		((eq tem '&lambda)
		 (throw 'exit
			(cond ((null p)
			       (list (+ sexp-column 4) containing-form-start))
			      ((null (cdr p))
                               ;; Indentation within a lambda-list. -- dvl
                               (list (lisp-indent-lambda-list
                                      indent-point
                                      sexp-column
                                      containing-form-start)
                                     containing-form-start))
                              (t
                               normal-indent))))
                ((integerp tem)
                 (throw 'exit
                   (if (null p)         ;not in subforms
                       (list (+ sexp-column tem) containing-form-start)
                       normal-indent)))
                ((symbolp tem)          ;a function to call
                 (throw 'exit
                   (funcall tem path state indent-point
                            sexp-column normal-indent)))
                (t
                 ;; must be a destructing frob
                 (if (not (null p))
                     ;; descend
               (setq method (cddr tem)
                           n nil)
               (setq tem (cadr tem))
                   (throw 'exit
                     (cond (tail
                            normal-indent)
                           ((eq tem 'nil)
                            (list normal-indent
                                  containing-form-start))
                           ((integerp tem)
                            (list (+ sexp-column tem)
                                  containing-form-start))
                           (t
                            (funcall tem path state indent-point
                                     sexp-column normal-indent))))))))))))

(defun lisp-indent-tagbody (path state indent-point sexp-column normal-indent)
  (if (not (null (cdr path)))
      normal-indent
    (save-excursion
      (goto-char indent-point)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (list (cond ((looking-at "\\sw\\|\\s_")
                   ;; a tagbody tag
                   (+ sexp-column lisp-tag-indentation))
                  ((integerp lisp-tag-body-indentation)
                   (+ sexp-column lisp-tag-body-indentation))
                  ((eq lisp-tag-body-indentation 't)
                   (condition-case ()
                       (progn (backward-sexp 1) (current-column))
                     (error (1+ sexp-column))))
                  (t (+ sexp-column lisp-body-indent)))
;            (cond ((integerp lisp-tag-body-indentation)
;                   (+ sexp-column lisp-tag-body-indentation))
;                  ((eq lisp-tag-body-indentation 't)
;                   normal-indent)
;                  (t
;                   (+ sexp-column lisp-body-indent)))
            (elt state 1)
            ))))

(defun lisp-indent-do (path state indent-point sexp-column normal-indent)
  (if (>= (car path) 3)
      (let ((lisp-tag-body-indentation lisp-body-indent))
        (funcall (function lisp-indent-tagbody)
                 path state indent-point sexp-column normal-indent))
    (funcall (function lisp-indent-259)
             '((&whole nil &rest
                ;; the following causes weird indentation
                ;;(&whole 1 1 2 nil)
                )
               (&whole nil &rest 1))
             path state indent-point sexp-column normal-indent)))


;; LISP-INDENT-DEFMETHOD now supports the presence of more than one method
;; qualifier and indents the method's lambda list properly. -- dvl
(defun lisp-indent-defmethod
    (path state indent-point sexp-column normal-indent)
  (lisp-indent-259
   (let ((nqual 0))
     (if (and (>= (car path) 3)
	      (save-excursion
		(beginning-of-defun)
		(forward-char 1)
		(forward-sexp 2)
		(skip-chars-forward " \t\n")
		(while (looking-at "\\sw\\|\\s_")
		  (incf nqual)
		  (forward-sexp)
		  (skip-chars-forward " \t\n"))
		(> nqual 0)))
         (append '(4) (make-list nqual 4) '(&lambda &body))
	 (get 'defun 'common-lisp-indent-function)))
   path state indent-point sexp-column normal-indent))


(defun lisp-indent-function-lambda-hack (path state indent-point
                                         sexp-column normal-indent)
  ;; indent (function (lambda () <newline> <body-forms>)) kludgily.
  (if (or (cdr path) ; wtf?
          (> (car path) 3))
      ;; line up under previous body form
      normal-indent
    ;; line up under function rather than under lambda in order to
    ;;  conserve horizontal space.  (Which is what #' is for.)
    (condition-case ()
        (save-excursion
          (backward-up-list 2)
          (forward-char 1)
          (if (looking-at "\\(lisp:+\\)?function\\(\\Sw\\|\\S_\\)")
              (+ lisp-body-indent -1 (current-column))
              (+ sexp-column lisp-body-indent)))
       (error (+ sexp-column lisp-body-indent)))))



(let ((l '((block 1)
           (case        (4 &rest (&whole 2 &rest 1)))
           (ccase . case)
           (ecase . case)
           (typecase . case)
           (etypecase . case)
           (ctypecase . case)
           (catch 1)
           (cond        (&rest (&whole 2 &rest 1)))
           (defvar      (4 2 2))
           (defclass    (6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)))
           (defconstant . defvar)
           (defcustom   (4 2 2 2))
           (defparameter . defvar)
           (defconst     . defcustom)
           (define-condition  . defclass)
           (define-modify-macro (4 &lambda &body))
           (defsetf     (4 &lambda 4 &body))
           (defun       (4 &lambda &body))
	   (defgeneric  (4 &lambda &body))
           (define-setf-method . defun)
           (define-setf-expander . defun)
           (defmacro . defun)
           (defsubst . defun)
           (deftype . defun)
	   (defmethod	lisp-indent-defmethod)
           (defpackage  (4 2))
           (defstruct   ((&whole 4 &rest (&whole 2 &rest 1))
                         &rest (&whole 2 &rest 1)))
           (destructuring-bind
                        ((&whole 6 &rest 1) 4 &body))
           (do          lisp-indent-do)
           (do* . do)
           (dolist      ((&whole 4 2 1) &body))
           (dotimes . dolist)
           (eval-when   1)
           (flet        ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
           (labels . flet)
           (macrolet . flet)
           (generic-flet . flet)
           (generic-labels . flet)
           (handler-case (4 &rest (&whole 2 &lambda &body)))
           (restart-case . handler-case)
           ;; `else-body' style
           (if          (nil nil &body))
           ;; single-else style (then and else equally indented)
           (if          (&rest nil))
           (lambda      (&lambda &rest lisp-indent-function-lambda-hack))
           (let         ((&whole 4 &rest (&whole 1 1 2)) &body))
           (let* . let)
           (compiler-let . let) ;barf
           (handler-bind . let)
           (restart-bind . let)
           (locally 1)
           ;(loop         lisp-indent-loop)
           (:method (&lambda &body)) ; in `defgeneric'
           (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
           (multiple-value-call (4 &body))
           (multiple-value-prog1 1)
           (multiple-value-setq (4 2))
           (multiple-value-setf . multiple-value-setq)
           (pprint-logical-block (4 2))
           (print-unreadable-object ((&whole 4 1 &rest 1) &body))
           ;; Combines the worst features of BLOCK, LET and TAGBODY
           (prog        (&lambda &rest lisp-indent-tagbody))
           (prog* . prog)
           (prog1 1)
           (prog2 2)
           (progn 0)
           (progv       (4 4 &body))
           (return 0)
           (return-from (nil &body))
           (symbol-macrolet . let)
           (tagbody     lisp-indent-tagbody)
           (throw 1)
           (unless 1)
           (unwind-protect (5 &body))
           (when 1)
           (with-accessors . multiple-value-bind)
           (with-condition-restarts . multiple-value-bind)
           (with-output-to-string (4 2))
           (with-slots . multiple-value-bind)
           (with-standard-io-syntax (2)))))
  (dolist (el l)
    (put (car el) 'common-lisp-indent-function
         (if (symbolp (cdr el))
             (get (cdr el) 'common-lisp-indent-function)
             (car (cdr el))))))


;(defun foo (x)
;  (tagbody
;   foo
;     (bar)
;   baz
;     (when (losing)
;       (with-big-loser
;           (yow)
;         ((lambda ()
;            foo)
;          big)))
;     (flet ((foo (bar baz zap)
;              (zip))
;            (zot ()
;              quux))
;       (do ()
;           ((lose)
;            (foo 1))
;         (quux)
;        foo
;         (lose))
;       (cond ((x)
;              (win 1 2
;                   (foo)))
;             (t
;              (lose
;                3))))))


;(put 'while    'common-lisp-indent-function 1)
;(put 'defwrapper'common-lisp-indent-function ...)
;(put 'def 'common-lisp-indent-function ...)
;(put 'defflavor        'common-lisp-indent-function ...)
;(put 'defsubst 'common-lisp-indent-function ...)

;(put 'with-restart 'common-lisp-indent-function '((1 4 ((* 1))) (2 &body)))
;(put 'restart-case 'common-lisp-indent-function '((1 4) (* 2 ((0 1) (* 1)))))
;(put 'define-condition 'common-lisp-indent-function '((1 6) (2 6 ((&whole 1))) (3 4 ((&whole 1))) (4 &body)))
;(put 'with-condition-handler 'common-lisp-indent-function '((1 4 ((* 1))) (2 &body)))
;(put 'condition-case 'common-lisp-indent-function '((1 4) (* 2 ((0 1) (1 3) (2 &body)))))
;(put 'defclass 'common-lisp-indent-function '((&whole 2 &rest (&whole 2 &rest 1) &rest (&whole 2 &rest 1)))
;(put 'defgeneric 'common-lisp-indent-function 'defun)

;;; cl-indent.el ends here
