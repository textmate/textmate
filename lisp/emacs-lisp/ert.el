;;; ert.el --- Emacs Lisp Regression Testing

;; Copyright (C) 2007-2008, 2010-2012 Free Software Foundation, Inc.

;; Author: Christian Ohler <ohler@gnu.org>
;; Keywords: lisp, tools

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; ERT is a tool for automated testing in Emacs Lisp.  Its main
;; features are facilities for defining and running test cases and
;; reporting the results as well as for debugging test failures
;; interactively.
;;
;; The main entry points are `ert-deftest', which is similar to
;; `defun' but defines a test, and `ert-run-tests-interactively',
;; which runs tests and offers an interactive interface for inspecting
;; results and debugging.  There is also
;; `ert-run-tests-batch-and-exit' for non-interactive use.
;;
;; The body of `ert-deftest' forms resembles a function body, but the
;; additional operators `should', `should-not' and `should-error' are
;; available.  `should' is similar to cl's `assert', but signals a
;; different error when its condition is violated that is caught and
;; processed by ERT.  In addition, it analyzes its argument form and
;; records information that helps debugging (`assert' tries to do
;; something similar when its second argument SHOW-ARGS is true, but
;; `should' is more sophisticated).  For information on `should-not'
;; and `should-error', see their docstrings.
;;
;; See ERT's info manual as well as the docstrings for more details.
;; To compile the manual, run `makeinfo ert.texinfo' in the ERT
;; directory, then C-u M-x info ert.info in Emacs to view it.
;;
;; To see some examples of tests written in ERT, see its self-tests in
;; ert-tests.el.  Some of these are tricky due to the bootstrapping
;; problem of writing tests for a testing tool, others test simple
;; functions and are straightforward.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'button)
(require 'debug)
(require 'easymenu)
(require 'ewoc)
(require 'find-func)
(require 'help)


;;; UI customization options.

(defgroup ert ()
  "ERT, the Emacs Lisp regression testing tool."
  :prefix "ert-"
  :group 'lisp)

(defface ert-test-result-expected '((((class color) (background light))
                                     :background "green1")
                                    (((class color) (background dark))
                                     :background "green3"))
  "Face used for expected results in the ERT results buffer."
  :group 'ert)

(defface ert-test-result-unexpected '((((class color) (background light))
                                       :background "red1")
                                      (((class color) (background dark))
                                       :background "red3"))
  "Face used for unexpected results in the ERT results buffer."
  :group 'ert)


;;; Copies/reimplementations of cl functions.

(defun ert--cl-do-remf (plist tag)
  "Copy of `cl-do-remf'.  Modify PLIST by removing TAG."
  (let ((p (cdr plist)))
    (while (and (cdr p) (not (eq (car (cdr p)) tag))) (setq p (cdr (cdr p))))
    (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t))))

(defun ert--remprop (sym tag)
  "Copy of `cl-remprop'.  Modify SYM's plist by removing TAG."
  (let ((plist (symbol-plist sym)))
    (if (and plist (eq tag (car plist)))
	(progn (setplist sym (cdr (cdr plist))) t)
      (ert--cl-do-remf plist tag))))

(defun ert--remove-if-not (ert-pred ert-list)
  "A reimplementation of `remove-if-not'.

ERT-PRED is a predicate, ERT-LIST is the input list."
  (loop for ert-x in ert-list
        if (funcall ert-pred ert-x)
        collect ert-x))

(defun ert--intersection (a b)
  "A reimplementation of `intersection'.  Intersect the sets A and B.

Elements are compared using `eql'."
  (loop for x in a
        if (memql x b)
        collect x))

(defun ert--set-difference (a b)
  "A reimplementation of `set-difference'.  Subtract the set B from the set A.

Elements are compared using `eql'."
  (loop for x in a
        unless (memql x b)
        collect x))

(defun ert--set-difference-eq (a b)
  "A reimplementation of `set-difference'.  Subtract the set B from the set A.

Elements are compared using `eq'."
  (loop for x in a
        unless (memq x b)
        collect x))

(defun ert--union (a b)
  "A reimplementation of `union'.  Compute the union of the sets A and B.

Elements are compared using `eql'."
  (append a (ert--set-difference b a)))

(eval-and-compile
  (defvar ert--gensym-counter 0))

(eval-and-compile
  (defun ert--gensym (&optional prefix)
    "Only allows string PREFIX, not compatible with CL."
    (unless prefix (setq prefix "G"))
    (make-symbol (format "%s%s"
                         prefix
                         (prog1 ert--gensym-counter
                           (incf ert--gensym-counter))))))

(defun ert--coerce-to-vector (x)
  "Coerce X to a vector."
  (when (char-table-p x) (error "Not supported"))
  (if (vectorp x)
      x
    (vconcat x)))

(defun* ert--remove* (x list &key key test)
  "Does not support all the keywords of remove*."
  (unless key (setq key #'identity))
  (unless test (setq test #'eql))
  (loop for y in list
        unless (funcall test x (funcall key y))
        collect y))

(defun ert--string-position (c s)
  "Return the position of the first occurrence of C in S, or nil if none."
  (loop for i from 0
        for x across s
        when (eql x c) return i))

(defun ert--mismatch (a b)
  "Return index of first element that differs between A and B.

Like `mismatch'.  Uses `equal' for comparison."
  (cond ((or (listp a) (listp b))
         (ert--mismatch (ert--coerce-to-vector a)
                        (ert--coerce-to-vector b)))
        ((> (length a) (length b))
         (ert--mismatch b a))
        (t
         (let ((la (length a))
               (lb (length b)))
           (assert (arrayp a) t)
           (assert (arrayp b) t)
           (assert (<= la lb) t)
           (loop for i below la
                 when (not (equal (aref a i) (aref b i))) return i
                 finally (return (if (/= la lb)
                                     la
                                   (assert (equal a b) t)
                                   nil)))))))

(defun ert--subseq (seq start &optional end)
  "Return a subsequence of SEQ from START to END."
  (when (char-table-p seq) (error "Not supported"))
  (let ((vector (substring (ert--coerce-to-vector seq) start end)))
    (etypecase seq
      (vector vector)
      (string (concat vector))
      (list (append vector nil))
      (bool-vector (loop with result = (make-bool-vector (length vector) nil)
                         for i below (length vector) do
                         (setf (aref result i) (aref vector i))
                         finally (return result)))
      (char-table (assert nil)))))

(defun ert-equal-including-properties (a b)
  "Return t if A and B have similar structure and contents.

This is like `equal-including-properties' except that it compares
the property values of text properties structurally (by
recursing) rather than with `eq'.  Perhaps this is what
`equal-including-properties' should do in the first place; see
Emacs bug 6581 at URL `http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6581'."
  ;; This implementation is inefficient.  Rather than making it
  ;; efficient, let's hope bug 6581 gets fixed so that we can delete
  ;; it altogether.
  (not (ert--explain-equal-including-properties a b)))


;;; Defining and locating tests.

;; The data structure that represents a test case.
(defstruct ert-test
  (name nil)
  (documentation nil)
  (body (assert nil))
  (most-recent-result nil)
  (expected-result-type ':passed)
  (tags '()))

(defun ert-test-boundp (symbol)
  "Return non-nil if SYMBOL names a test."
  (and (get symbol 'ert--test) t))

(defun ert-get-test (symbol)
  "If SYMBOL names a test, return that.  Signal an error otherwise."
  (unless (ert-test-boundp symbol) (error "No test named `%S'" symbol))
  (get symbol 'ert--test))

(defun ert-set-test (symbol definition)
  "Make SYMBOL name the test DEFINITION, and return DEFINITION."
  (when (eq symbol 'nil)
    ;; We disallow nil since `ert-test-at-point' and related functions
    ;; want to return a test name, but also need an out-of-band value
    ;; on failure.  Nil is the most natural out-of-band value; using 0
    ;; or "" or signaling an error would be too awkward.
    ;;
    ;; Note that nil is still a valid value for the `name' slot in
    ;; ert-test objects.  It designates an anonymous test.
    (error "Attempt to define a test named nil"))
  (put symbol 'ert--test definition)
  definition)

(defun ert-make-test-unbound (symbol)
  "Make SYMBOL name no test.  Return SYMBOL."
  (ert--remprop symbol 'ert--test)
  symbol)

(defun ert--parse-keys-and-body (keys-and-body)
  "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body."
  (let ((extracted-key-accu '())
        (remaining keys-and-body))
    (while (and (consp remaining) (keywordp (first remaining)))
      (let ((keyword (pop remaining)))
        (unless (consp remaining)
          (error "Value expected after keyword %S in %S"
                 keyword keys-and-body))
        (when (assoc keyword extracted-key-accu)
          (warn "Keyword %S appears more than once in %S" keyword
                keys-and-body))
        (push (cons keyword (pop remaining)) extracted-key-accu)))
    (setq extracted-key-accu (nreverse extracted-key-accu))
    (list (loop for (key . value) in extracted-key-accu
                collect key
                collect value)
          remaining)))

;;;###autoload
(defmacro* ert-deftest (name () &body docstring-keys-and-body)
  "Define NAME (a symbol) as a test.

BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not' and `should-error' are useful for
assertions in BODY.

Use `ert' to run tests interactively.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
\[:tags '(TAG...)] BODY...)"
  (declare (debug (&define :name test
                           name sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  (let ((documentation nil)
        (documentation-supplied-p nil))
    (when (stringp (first docstring-keys-and-body))
      (setq documentation (pop docstring-keys-and-body)
            documentation-supplied-p t))
    (destructuring-bind ((&key (expected-result nil expected-result-supplied-p)
                               (tags nil tags-supplied-p))
                         body)
        (ert--parse-keys-and-body docstring-keys-and-body)
      `(progn
         (ert-set-test ',name
                       (make-ert-test
                        :name ',name
                        ,@(when documentation-supplied-p
                            `(:documentation ,documentation))
                        ,@(when expected-result-supplied-p
                            `(:expected-result-type ,expected-result))
                        ,@(when tags-supplied-p
                            `(:tags ,tags))
                        :body (lambda () ,@body)))
         ;; This hack allows `symbol-file' to associate `ert-deftest'
         ;; forms with files, and therefore enables `find-function' to
         ;; work with tests.  However, it leads to warnings in
         ;; `unload-feature', which doesn't know how to undefine tests
         ;; and has no mechanism for extension.
         (push '(ert-deftest . ,name) current-load-list)
         ',name))))

;; We use these `put' forms in addition to the (declare (indent)) in
;; the defmacro form since the `declare' alone does not lead to
;; correct indentation before the .el/.elc file is loaded.
;; Autoloading these `put' forms solves this.
;;;###autoload
(progn
  ;; TODO(ohler): Figure out what these mean and make sure they are correct.
  (put 'ert-deftest 'lisp-indent-function 2)
  (put 'ert-info 'lisp-indent-function 1))

(defvar ert--find-test-regexp
  (concat "^\\s-*(ert-deftest"
          find-function-space-re
          "%s\\(\\s-\\|$\\)")
  "The regexp the `find-function' mechanisms use for finding test definitions.")


(put 'ert-test-failed 'error-conditions '(error ert-test-failed))
(put 'ert-test-failed 'error-message "Test failed")

(defun ert-pass ()
  "Terminate the current test and mark it passed.  Does not return."
  (throw 'ert--pass nil))

(defun ert-fail (data)
  "Terminate the current test and mark it failed.  Does not return.
DATA is displayed to the user and should state the reason of the failure."
  (signal 'ert-test-failed (list data)))


;;; The `should' macros.

(defvar ert--should-execution-observer nil)

(defun ert--signal-should-execution (form-description)
  "Tell the current `should' form observer (if any) about FORM-DESCRIPTION."
  (when ert--should-execution-observer
    (funcall ert--should-execution-observer form-description)))

(defun ert--special-operator-p (thing)
  "Return non-nil if THING is a symbol naming a special operator."
  (and (symbolp thing)
       (let ((definition (indirect-function thing t)))
         (and (subrp definition)
              (eql (cdr (subr-arity definition)) 'unevalled)))))

(defun ert--expand-should-1 (whole form inner-expander)
  "Helper function for the `should' macro and its variants."
  (let ((form
         ;; If `cl-macroexpand' isn't bound, the code that we're
         ;; compiling doesn't depend on cl and thus doesn't need an
         ;; environment arg for `macroexpand'.
         (if (fboundp 'cl-macroexpand)
             ;; Suppress warning about run-time call to cl function: we
             ;; only call it if it's fboundp.
             (with-no-warnings
               (cl-macroexpand form (and (boundp 'cl-macro-environment)
                                         cl-macro-environment)))
           (macroexpand form))))
    (cond
     ((or (atom form) (ert--special-operator-p (car form)))
      (let ((value (ert--gensym "value-")))
        `(let ((,value (ert--gensym "ert-form-evaluation-aborted-")))
           ,(funcall inner-expander
                     `(setq ,value ,form)
                     `(list ',whole :form ',form :value ,value)
                     value)
           ,value)))
     (t
      (let ((fn-name (car form))
            (arg-forms (cdr form)))
        (assert (or (symbolp fn-name)
                    (and (consp fn-name)
                         (eql (car fn-name) 'lambda)
                         (listp (cdr fn-name)))))
        (let ((fn (ert--gensym "fn-"))
              (args (ert--gensym "args-"))
              (value (ert--gensym "value-"))
              (default-value (ert--gensym "ert-form-evaluation-aborted-")))
          `(let ((,fn (function ,fn-name))
                 (,args (list ,@arg-forms)))
             (let ((,value ',default-value))
               ,(funcall inner-expander
                         `(setq ,value (apply ,fn ,args))
                         `(nconc (list ',whole)
                                 (list :form `(,,fn ,@,args))
                                 (unless (eql ,value ',default-value)
                                   (list :value ,value))
                                 (let ((-explainer-
                                        (and (symbolp ',fn-name)
                                             (get ',fn-name 'ert-explainer))))
                                   (when -explainer-
                                     (list :explanation
                                           (apply -explainer- ,args)))))
                         value)
               ,value))))))))

(defun ert--expand-should (whole form inner-expander)
  "Helper function for the `should' macro and its variants.

Analyzes FORM and returns an expression that has the same
semantics under evaluation but records additional debugging
information.

INNER-EXPANDER should be a function and is called with two
arguments: INNER-FORM and FORM-DESCRIPTION-FORM, where INNER-FORM
is an expression equivalent to FORM, and FORM-DESCRIPTION-FORM is
an expression that returns a description of FORM.  INNER-EXPANDER
should return code that calls INNER-FORM and performs the checks
and error signaling specific to the particular variant of
`should'.  The code that INNER-EXPANDER returns must not call
FORM-DESCRIPTION-FORM before it has called INNER-FORM."
  (lexical-let ((inner-expander inner-expander))
    (ert--expand-should-1
     whole form
     (lambda (inner-form form-description-form value-var)
       (let ((form-description (ert--gensym "form-description-")))
         `(let (,form-description)
            ,(funcall inner-expander
                      `(unwind-protect
                           ,inner-form
                         (setq ,form-description ,form-description-form)
                         (ert--signal-should-execution ,form-description))
                      `,form-description
                      value-var)))))))

(defmacro* should (form)
  "Evaluate FORM.  If it returns nil, abort the current test as failed.

Returns the value of FORM."
  (ert--expand-should `(should ,form) form
                      (lambda (inner-form form-description-form value-var)
                        `(unless ,inner-form
                           (ert-fail ,form-description-form)))))

(defmacro* should-not (form)
  "Evaluate FORM.  If it returns non-nil, abort the current test as failed.

Returns nil."
  (ert--expand-should `(should-not ,form) form
                      (lambda (inner-form form-description-form value-var)
                        `(unless (not ,inner-form)
                           (ert-fail ,form-description-form)))))

(defun ert--should-error-handle-error (form-description-fn
                                       condition type exclude-subtypes)
  "Helper function for `should-error'.

Determines whether CONDITION matches TYPE and EXCLUDE-SUBTYPES,
and aborts the current test as failed if it doesn't."
  (let ((signaled-conditions (get (car condition) 'error-conditions))
        (handled-conditions (etypecase type
                              (list type)
                              (symbol (list type)))))
    (assert signaled-conditions)
    (unless (ert--intersection signaled-conditions handled-conditions)
      (ert-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason (concat "the error signaled did not"
                                       " have the expected type")))))
    (when exclude-subtypes
      (unless (member (car condition) handled-conditions)
        (ert-fail (append
                   (funcall form-description-fn)
                   (list
                    :condition condition
                    :fail-reason (concat "the error signaled was a subtype"
                                         " of the expected type"))))))))

;; FIXME: The expansion will evaluate the keyword args (if any) in
;; nonstandard order.
(defmacro* should-error (form &rest keys &key type exclude-subtypes)
  "Evaluate FORM and check that it signals an error.

The error signaled needs to match TYPE.  TYPE should be a list
of condition names.  (It can also be a non-nil symbol, which is
equivalent to a singleton list containing that symbol.)  If
EXCLUDE-SUBTYPES is nil, the error matches TYPE if one of its
condition names is an element of TYPE.  If EXCLUDE-SUBTYPES is
non-nil, the error matches TYPE if it is an element of TYPE.

If the error matches, returns (ERROR-SYMBOL . DATA) from the
error.  If not, or if no error was signaled, abort the test as
failed."
  (unless type (setq type ''error))
  (ert--expand-should
   `(should-error ,form ,@keys)
   form
   (lambda (inner-form form-description-form value-var)
     (let ((errorp (ert--gensym "errorp"))
           (form-description-fn (ert--gensym "form-description-fn-")))
       `(let ((,errorp nil)
              (,form-description-fn (lambda () ,form-description-form)))
          (condition-case -condition-
              ,inner-form
            ;; We can't use ,type here because we want to evaluate it.
            (error
             (setq ,errorp t)
             (ert--should-error-handle-error ,form-description-fn
                                             -condition-
                                             ,type ,exclude-subtypes)
             (setq ,value-var -condition-)))
          (unless ,errorp
            (ert-fail (append
                       (funcall ,form-description-fn)
                       (list
                        :fail-reason "did not signal an error")))))))))


;;; Explanation of `should' failures.

;; TODO(ohler): Rework explanations so that they are displayed in a
;; similar way to `ert-info' messages; in particular, allow text
;; buttons in explanations that give more detail or open an ediff
;; buffer.  Perhaps explanations should be reported through `ert-info'
;; rather than as part of the condition.

(defun ert--proper-list-p (x)
  "Return non-nil if X is a proper list, nil otherwise."
  (loop
   for firstp = t then nil
   for fast = x then (cddr fast)
   for slow = x then (cdr slow) do
   (when (null fast) (return t))
   (when (not (consp fast)) (return nil))
   (when (null (cdr fast)) (return t))
   (when (not (consp (cdr fast))) (return nil))
   (when (and (not firstp) (eq fast slow)) (return nil))))

(defun ert--explain-format-atom (x)
  "Format the atom X for `ert--explain-equal'."
  (typecase x
    (fixnum (list x (format "#x%x" x) (format "?%c" x)))
    (t x)))

(defun ert--explain-equal-rec (a b)
  "Return a programmer-readable explanation of why A and B are not `equal'.
Returns nil if they are."
  (if (not (equal (type-of a) (type-of b)))
      `(different-types ,a ,b)
    (etypecase a
      (cons
       (let ((a-proper-p (ert--proper-list-p a))
             (b-proper-p (ert--proper-list-p b)))
         (if (not (eql (not a-proper-p) (not b-proper-p)))
             `(one-list-proper-one-improper ,a ,b)
           (if a-proper-p
               (if (not (equal (length a) (length b)))
                   `(proper-lists-of-different-length ,(length a) ,(length b)
                                                      ,a ,b
                                                      first-mismatch-at
                                                      ,(ert--mismatch a b))
                 (loop for i from 0
                       for ai in a
                       for bi in b
                       for xi = (ert--explain-equal-rec ai bi)
                       do (when xi (return `(list-elt ,i ,xi)))
                       finally (assert (equal a b) t)))
             (let ((car-x (ert--explain-equal-rec (car a) (car b))))
               (if car-x
                   `(car ,car-x)
                 (let ((cdr-x (ert--explain-equal-rec (cdr a) (cdr b))))
                   (if cdr-x
                       `(cdr ,cdr-x)
                     (assert (equal a b) t)
                     nil))))))))
      (array (if (not (equal (length a) (length b)))
                 `(arrays-of-different-length ,(length a) ,(length b)
                                              ,a ,b
                                              ,@(unless (char-table-p a)
                                                  `(first-mismatch-at
                                                    ,(ert--mismatch a b))))
               (loop for i from 0
                     for ai across a
                     for bi across b
                     for xi = (ert--explain-equal-rec ai bi)
                     do (when xi (return `(array-elt ,i ,xi)))
                     finally (assert (equal a b) t))))
      (atom (if (not (equal a b))
                (if (and (symbolp a) (symbolp b) (string= a b))
                    `(different-symbols-with-the-same-name ,a ,b)
                  `(different-atoms ,(ert--explain-format-atom a)
                                    ,(ert--explain-format-atom b)))
              nil)))))

(defun ert--explain-equal (a b)
  "Explainer function for `equal'."
  ;; Do a quick comparison in C to avoid running our expensive
  ;; comparison when possible.
  (if (equal a b)
      nil
    (ert--explain-equal-rec a b)))
(put 'equal 'ert-explainer 'ert--explain-equal)

(defun ert--significant-plist-keys (plist)
  "Return the keys of PLIST that have non-null values, in order."
  (assert (zerop (mod (length plist) 2)) t)
  (loop for (key value . rest) on plist by #'cddr
        unless (or (null value) (memq key accu)) collect key into accu
        finally (return accu)))

(defun ert--plist-difference-explanation (a b)
  "Return a programmer-readable explanation of why A and B are different plists.

Returns nil if they are equivalent, i.e., have the same value for
each key, where absent values are treated as nil.  The order of
key/value pairs in each list does not matter."
  (assert (zerop (mod (length a) 2)) t)
  (assert (zerop (mod (length b) 2)) t)
  ;; Normalizing the plists would be another way to do this but it
  ;; requires a total ordering on all lisp objects (since any object
  ;; is valid as a text property key).  Perhaps defining such an
  ;; ordering is useful in other contexts, too, but it's a lot of
  ;; work, so let's punt on it for now.
  (let* ((keys-a (ert--significant-plist-keys a))
         (keys-b (ert--significant-plist-keys b))
         (keys-in-a-not-in-b (ert--set-difference-eq keys-a keys-b))
         (keys-in-b-not-in-a (ert--set-difference-eq keys-b keys-a)))
    (flet ((explain-with-key (key)
             (let ((value-a (plist-get a key))
                   (value-b (plist-get b key)))
               (assert (not (equal value-a value-b)) t)
               `(different-properties-for-key
                 ,key ,(ert--explain-equal-including-properties value-a
                                                                value-b)))))
      (cond (keys-in-a-not-in-b
             (explain-with-key (first keys-in-a-not-in-b)))
            (keys-in-b-not-in-a
             (explain-with-key (first keys-in-b-not-in-a)))
            (t
             (loop for key in keys-a
                   when (not (equal (plist-get a key) (plist-get b key)))
                   return (explain-with-key key)))))))

(defun ert--abbreviate-string (s len suffixp)
  "Shorten string S to at most LEN chars.

If SUFFIXP is non-nil, returns a suffix of S, otherwise a prefix."
  (let ((n (length s)))
    (cond ((< n len)
           s)
          (suffixp
           (substring s (- n len)))
          (t
           (substring s 0 len)))))

;; TODO(ohler): Once bug 6581 is fixed, rename this to
;; `ert--explain-equal-including-properties-rec' and add a fast-path
;; wrapper like `ert--explain-equal'.
(defun ert--explain-equal-including-properties (a b)
  "Explainer function for `ert-equal-including-properties'.

Returns a programmer-readable explanation of why A and B are not
`ert-equal-including-properties', or nil if they are."
  (if (not (equal a b))
      (ert--explain-equal a b)
    (assert (stringp a) t)
    (assert (stringp b) t)
    (assert (eql (length a) (length b)) t)
    (loop for i from 0 to (length a)
          for props-a = (text-properties-at i a)
          for props-b = (text-properties-at i b)
          for difference = (ert--plist-difference-explanation props-a props-b)
          do (when difference
               (return `(char ,i ,(substring-no-properties a i (1+ i))
                              ,difference
                              context-before
                              ,(ert--abbreviate-string
                                (substring-no-properties a 0 i)
                                10 t)
                              context-after
                              ,(ert--abbreviate-string
                                (substring-no-properties a (1+ i))
                                10 nil))))
          ;; TODO(ohler): Get `equal-including-properties' fixed in
          ;; Emacs, delete `ert-equal-including-properties', and
          ;; re-enable this assertion.
          ;;finally (assert (equal-including-properties a b) t)
          )))
(put 'ert-equal-including-properties
     'ert-explainer
     'ert--explain-equal-including-properties)


;;; Implementation of `ert-info'.

;; TODO(ohler): The name `info' clashes with
;; `ert--test-execution-info'.  One or both should be renamed.
(defvar ert--infos '()
  "The stack of `ert-info' infos that currently apply.

Bound dynamically.  This is a list of (PREFIX . MESSAGE) pairs.")

(defmacro* ert-info ((message-form &key ((:prefix prefix-form) "Info: "))
                     &body body)
  "Evaluate MESSAGE-FORM and BODY, and report the message if BODY fails.

To be used within ERT tests.  MESSAGE-FORM should evaluate to a
string that will be displayed together with the test result if
the test fails.  PREFIX-FORM should evaluate to a string as well
and is displayed in front of the value of MESSAGE-FORM."
  (declare (debug ((form &rest [sexp form]) body))
	   (indent 1))
  `(let ((ert--infos (cons (cons ,prefix-form ,message-form) ert--infos)))
     ,@body))



;;; Facilities for running a single test.

(defvar ert-debug-on-error nil
  "Non-nil means enter debugger when a test fails or terminates with an error.")

;; The data structures that represent the result of running a test.
(defstruct ert-test-result
  (messages nil)
  (should-forms nil)
  )
(defstruct (ert-test-passed (:include ert-test-result)))
(defstruct (ert-test-result-with-condition (:include ert-test-result))
  (condition (assert nil))
  (backtrace (assert nil))
  (infos (assert nil)))
(defstruct (ert-test-quit (:include ert-test-result-with-condition)))
(defstruct (ert-test-failed (:include ert-test-result-with-condition)))
(defstruct (ert-test-aborted-with-non-local-exit (:include ert-test-result)))


(defun ert--record-backtrace ()
  "Record the current backtrace (as a list) and return it."
  ;; Since the backtrace is stored in the result object, result
  ;; objects must only be printed with appropriate limits
  ;; (`print-level' and `print-length') in place.  For interactive
  ;; use, the cost of ensuring this possibly outweighs the advantage
  ;; of storing the backtrace for
  ;; `ert-results-pop-to-backtrace-for-test-at-point' given that we
  ;; already have `ert-results-rerun-test-debugging-errors-at-point'.
  ;; For batch use, however, printing the backtrace may be useful.
  (loop
   ;; 6 is the number of frames our own debugger adds (when
   ;; compiled; more when interpreted).  FIXME: Need to describe a
   ;; procedure for determining this constant.
   for i from 6
   for frame = (backtrace-frame i)
   while frame
   collect frame))

(defun ert--print-backtrace (backtrace)
  "Format the backtrace BACKTRACE to the current buffer."
  ;; This is essentially a reimplementation of Fbacktrace
  ;; (src/eval.c), but for a saved backtrace, not the current one.
  (let ((print-escape-newlines t)
        (print-level 8)
        (print-length 50))
    (dolist (frame backtrace)
      (ecase (first frame)
        ((nil)
         ;; Special operator.
         (destructuring-bind (special-operator &rest arg-forms)
             (cdr frame)
           (insert
            (format "  %S\n" (list* special-operator arg-forms)))))
        ((t)
         ;; Function call.
         (destructuring-bind (fn &rest args) (cdr frame)
           (insert (format "  %S(" fn))
           (loop for firstp = t then nil
                 for arg in args do
                 (unless firstp
                   (insert " "))
                 (insert (format "%S" arg)))
           (insert ")\n")))))))

;; A container for the state of the execution of a single test and
;; environment data needed during its execution.
(defstruct ert--test-execution-info
  (test (assert nil))
  (result (assert nil))
  ;; A thunk that may be called when RESULT has been set to its final
  ;; value and test execution should be terminated.  Should not
  ;; return.
  (exit-continuation (assert nil))
  ;; The binding of `debugger' outside of the execution of the test.
  next-debugger
  ;; The binding of `ert-debug-on-error' that is in effect for the
  ;; execution of the current test.  We store it to avoid being
  ;; affected by any new bindings the test itself may establish.  (I
  ;; don't remember whether this feature is important.)
  ert-debug-on-error)

(defun ert--run-test-debugger (info debugger-args)
  "During a test run, `debugger' is bound to a closure that calls this function.

This function records failures and errors and either terminates
the test silently or calls the interactive debugger, as
appropriate.

INFO is the ert--test-execution-info corresponding to this test
run.  DEBUGGER-ARGS are the arguments to `debugger'."
  (destructuring-bind (first-debugger-arg &rest more-debugger-args)
      debugger-args
    (ecase first-debugger-arg
      ((lambda debug t exit nil)
       (apply (ert--test-execution-info-next-debugger info) debugger-args))
      (error
       (let* ((condition (first more-debugger-args))
              (type (case (car condition)
                      ((quit) 'quit)
                      (otherwise 'failed)))
              (backtrace (ert--record-backtrace))
              (infos (reverse ert--infos)))
         (setf (ert--test-execution-info-result info)
               (ecase type
                 (quit
                  (make-ert-test-quit :condition condition
                                      :backtrace backtrace
                                      :infos infos))
                 (failed
                  (make-ert-test-failed :condition condition
                                        :backtrace backtrace
                                        :infos infos))))
         ;; Work around Emacs's heuristic (in eval.c) for detecting
         ;; errors in the debugger.
         (incf num-nonmacro-input-events)
         ;; FIXME: We should probably implement more fine-grained
         ;; control a la non-t `debug-on-error' here.
         (cond
          ((ert--test-execution-info-ert-debug-on-error info)
           (apply (ert--test-execution-info-next-debugger info) debugger-args))
          (t))
         (funcall (ert--test-execution-info-exit-continuation info)))))))

(defun ert--run-test-internal (ert-test-execution-info)
  "Low-level function to run a test according to ERT-TEST-EXECUTION-INFO.

This mainly sets up debugger-related bindings."
  (lexical-let ((info ert-test-execution-info))
    (setf (ert--test-execution-info-next-debugger info) debugger
          (ert--test-execution-info-ert-debug-on-error info) ert-debug-on-error)
    (catch 'ert--pass
      ;; For now, each test gets its own temp buffer and its own
      ;; window excursion, just to be safe.  If this turns out to be
      ;; too expensive, we can remove it.
      (with-temp-buffer
        (save-window-excursion
          (let ((debugger (lambda (&rest debugger-args)
                            (ert--run-test-debugger info debugger-args)))
                (debug-on-error t)
                (debug-on-quit t)
                ;; FIXME: Do we need to store the old binding of this
                ;; and consider it in `ert--run-test-debugger'?
                (debug-ignored-errors nil)
                (ert--infos '()))
            (funcall (ert-test-body (ert--test-execution-info-test info))))))
      (ert-pass))
    (setf (ert--test-execution-info-result info) (make-ert-test-passed)))
  nil)

(defun ert--force-message-log-buffer-truncation ()
  "Immediately truncate *Messages* buffer according to `message-log-max'.

This can be useful after reducing the value of `message-log-max'."
  (with-current-buffer (get-buffer-create "*Messages*")
    ;; This is a reimplementation of this part of message_dolog() in xdisp.c:
    ;; if (NATNUMP (Vmessage_log_max))
    ;;   {
    ;;     scan_newline (Z, Z_BYTE, BEG, BEG_BYTE,
    ;;                   -XFASTINT (Vmessage_log_max) - 1, 0);
    ;;     del_range_both (BEG, BEG_BYTE, PT, PT_BYTE, 0);
    ;;   }
    (when (and (integerp message-log-max) (>= message-log-max 0))
      (let ((begin (point-min))
            (end (save-excursion
                   (goto-char (point-max))
                   (forward-line (- message-log-max))
                   (point))))
        (delete-region begin end)))))

(defvar ert--running-tests nil
  "List of tests that are currently in execution.

This list is empty while no test is running, has one element
while a test is running, two elements while a test run from
inside a test is running, etc.  The list is in order of nesting,
innermost test first.

The elements are of type `ert-test'.")

(defun ert-run-test (ert-test)
  "Run ERT-TEST.

Returns the result and stores it in ERT-TEST's `most-recent-result' slot."
  (setf (ert-test-most-recent-result ert-test) nil)
  (block error
    (lexical-let ((begin-marker
                   (with-current-buffer (get-buffer-create "*Messages*")
                     (set-marker (make-marker) (point-max)))))
      (unwind-protect
          (lexical-let ((info (make-ert--test-execution-info
                               :test ert-test
                               :result
                               (make-ert-test-aborted-with-non-local-exit)
                               :exit-continuation (lambda ()
                                                    (return-from error nil))))
                        (should-form-accu (list)))
            (unwind-protect
                (let ((ert--should-execution-observer
                       (lambda (form-description)
                         (push form-description should-form-accu)))
                      (message-log-max t)
                      (ert--running-tests (cons ert-test ert--running-tests)))
                  (ert--run-test-internal info))
              (let ((result (ert--test-execution-info-result info)))
                (setf (ert-test-result-messages result)
                      (with-current-buffer (get-buffer-create "*Messages*")
                        (buffer-substring begin-marker (point-max))))
                (ert--force-message-log-buffer-truncation)
                (setq should-form-accu (nreverse should-form-accu))
                (setf (ert-test-result-should-forms result)
                      should-form-accu)
                (setf (ert-test-most-recent-result ert-test) result))))
        (set-marker begin-marker nil))))
  (ert-test-most-recent-result ert-test))

(defun ert-running-test ()
  "Return the top-level test currently executing."
  (car (last ert--running-tests)))


;;; Test selectors.

(defun ert-test-result-type-p (result result-type)
  "Return non-nil if RESULT matches type RESULT-TYPE.

Valid result types:

nil -- Never matches.
t -- Always matches.
:failed, :passed -- Matches corresponding results.
\(and TYPES...\) -- Matches if all TYPES match.
\(or TYPES...\) -- Matches if some TYPES match.
\(not TYPE\) -- Matches if TYPE does not match.
\(satisfies PREDICATE\) -- Matches if PREDICATE returns true when called with
                           RESULT."
  ;; It would be easy to add `member' and `eql' types etc., but I
  ;; haven't bothered yet.
  (etypecase result-type
    ((member nil) nil)
    ((member t) t)
    ((member :failed) (ert-test-failed-p result))
    ((member :passed) (ert-test-passed-p result))
    (cons
     (destructuring-bind (operator &rest operands) result-type
       (ecase operator
         (and
          (case (length operands)
            (0 t)
            (t
             (and (ert-test-result-type-p result (first operands))
                  (ert-test-result-type-p result `(and ,@(rest operands)))))))
         (or
          (case (length operands)
            (0 nil)
            (t
             (or (ert-test-result-type-p result (first operands))
                 (ert-test-result-type-p result `(or ,@(rest operands)))))))
         (not
          (assert (eql (length operands) 1))
          (not (ert-test-result-type-p result (first operands))))
         (satisfies
          (assert (eql (length operands) 1))
          (funcall (first operands) result)))))))

(defun ert-test-result-expected-p (test result)
  "Return non-nil if TEST's expected result type matches RESULT."
  (ert-test-result-type-p result (ert-test-expected-result-type test)))

(defun ert-select-tests (selector universe)
  "Return a list of tests that match SELECTOR.

UNIVERSE specifies the set of tests to select from; it should be a list
of tests, or t, which refers to all tests named by symbols in `obarray'.

Valid SELECTORs:

nil  -- Selects the empty set.
t    -- Selects UNIVERSE.
:new -- Selects all tests that have not been run yet.
:failed, :passed       -- Select tests according to their most recent result.
:expected, :unexpected -- Select tests according to their most recent result.
a string -- A regular expression selecting all tests with matching names.
a test   -- (i.e., an object of the ert-test data-type) Selects that test.
a symbol -- Selects the test that the symbol names, errors if none.
\(member TESTS...) -- Selects the elements of TESTS, a list of tests
    or symbols naming tests.
\(eql TEST\) -- Selects TEST, a test or a symbol naming a test.
\(and SELECTORS...) -- Selects the tests that match all SELECTORS.
\(or SELECTORS...)  -- Selects the tests that match any of the SELECTORS.
\(not SELECTOR)     -- Selects all tests that do not match SELECTOR.
\(tag TAG) -- Selects all tests that have TAG on their tags list.
    A tag is an arbitrary label you can apply when you define a test.
\(satisfies PREDICATE) -- Selects all tests that satisfy PREDICATE.
    PREDICATE is a function that takes an ert-test object as argument,
    and returns non-nil if it is selected.

Only selectors that require a superset of tests, such
as (satisfies ...), strings, :new, etc. make use of UNIVERSE.
Selectors that do not, such as (member ...), just return the
set implied by them without checking whether it is really
contained in UNIVERSE."
  ;; This code needs to match the etypecase in
  ;; `ert-insert-human-readable-selector'.
  (etypecase selector
    ((member nil) nil)
    ((member t) (etypecase universe
                  (list universe)
                  ((member t) (ert-select-tests "" universe))))
    ((member :new) (ert-select-tests
                    `(satisfies ,(lambda (test)
                                   (null (ert-test-most-recent-result test))))
                    universe))
    ((member :failed) (ert-select-tests
                       `(satisfies ,(lambda (test)
                                      (ert-test-result-type-p
                                       (ert-test-most-recent-result test)
                                       ':failed)))
                       universe))
    ((member :passed) (ert-select-tests
                       `(satisfies ,(lambda (test)
                                      (ert-test-result-type-p
                                       (ert-test-most-recent-result test)
                                       ':passed)))
                       universe))
    ((member :expected) (ert-select-tests
                         `(satisfies
                           ,(lambda (test)
                              (ert-test-result-expected-p
                               test
                               (ert-test-most-recent-result test))))
                         universe))
    ((member :unexpected) (ert-select-tests `(not :expected) universe))
    (string
     (etypecase universe
       ((member t) (mapcar #'ert-get-test
                           (apropos-internal selector #'ert-test-boundp)))
       (list (ert--remove-if-not (lambda (test)
                                   (and (ert-test-name test)
                                        (string-match selector
                                                      (ert-test-name test))))
                                 universe))))
    (ert-test (list selector))
    (symbol
     (assert (ert-test-boundp selector))
     (list (ert-get-test selector)))
    (cons
     (destructuring-bind (operator &rest operands) selector
       (ecase operator
         (member
          (mapcar (lambda (purported-test)
                    (etypecase purported-test
                      (symbol (assert (ert-test-boundp purported-test))
                              (ert-get-test purported-test))
                      (ert-test purported-test)))
                  operands))
         (eql
          (assert (eql (length operands) 1))
          (ert-select-tests `(member ,@operands) universe))
         (and
          ;; Do these definitions of AND, NOT and OR satisfy de
          ;; Morgan's laws?  Should they?
          (case (length operands)
            (0 (ert-select-tests 't universe))
            (t (ert-select-tests `(and ,@(rest operands))
                                 (ert-select-tests (first operands)
                                                   universe)))))
         (not
          (assert (eql (length operands) 1))
          (let ((all-tests (ert-select-tests 't universe)))
            (ert--set-difference all-tests
                                 (ert-select-tests (first operands)
                                                   all-tests))))
         (or
          (case (length operands)
            (0 (ert-select-tests 'nil universe))
            (t (ert--union (ert-select-tests (first operands) universe)
                           (ert-select-tests `(or ,@(rest operands))
                                             universe)))))
         (tag
          (assert (eql (length operands) 1))
          (let ((tag (first operands)))
            (ert-select-tests `(satisfies
                                ,(lambda (test)
                                   (member tag (ert-test-tags test))))
                              universe)))
         (satisfies
          (assert (eql (length operands) 1))
          (ert--remove-if-not (first operands)
                              (ert-select-tests 't universe))))))))

(defun ert--insert-human-readable-selector (selector)
  "Insert a human-readable presentation of SELECTOR into the current buffer."
  ;; This is needed to avoid printing the (huge) contents of the
  ;; `backtrace' slot of the result objects in the
  ;; `most-recent-result' slots of test case objects in (eql ...) or
  ;; (member ...) selectors.
  (labels ((rec (selector)
             ;; This code needs to match the etypecase in `ert-select-tests'.
             (etypecase selector
               ((or (member nil t
                            :new :failed :passed
                            :expected :unexpected)
                    string
                    symbol)
                selector)
               (ert-test
                (if (ert-test-name selector)
                    (make-symbol (format "<%S>" (ert-test-name selector)))
                  (make-symbol "<unnamed test>")))
               (cons
                (destructuring-bind (operator &rest operands) selector
                  (ecase operator
                    ((member eql and not or)
                     `(,operator ,@(mapcar #'rec operands)))
                    ((member tag satisfies)
                     selector)))))))
    (insert (format "%S" (rec selector)))))


;;; Facilities for running a whole set of tests.

;; The data structure that contains the set of tests being executed
;; during one particular test run, their results, the state of the
;; execution, and some statistics.
;;
;; The data about results and expected results of tests may seem
;; redundant here, since the test objects also carry such information.
;; However, the information in the test objects may be more recent, it
;; may correspond to a different test run.  We need the information
;; that corresponds to this run in order to be able to update the
;; statistics correctly when a test is re-run interactively and has a
;; different result than before.
(defstruct ert--stats
  (selector (assert nil))
  ;; The tests, in order.
  (tests (assert nil) :type vector)
  ;; A map of test names (or the test objects themselves for unnamed
  ;; tests) to indices into the `tests' vector.
  (test-map (assert nil) :type hash-table)
  ;; The results of the tests during this run, in order.
  (test-results (assert nil) :type vector)
  ;; The start times of the tests, in order, as reported by
  ;; `current-time'.
  (test-start-times (assert nil) :type vector)
  ;; The end times of the tests, in order, as reported by
  ;; `current-time'.
  (test-end-times (assert nil) :type vector)
  (passed-expected 0)
  (passed-unexpected 0)
  (failed-expected 0)
  (failed-unexpected 0)
  (start-time nil)
  (end-time nil)
  (aborted-p nil)
  (current-test nil)
  ;; The time at or after which the next redisplay should occur, as a
  ;; float.
  (next-redisplay 0.0))

(defun ert-stats-completed-expected (stats)
  "Return the number of tests in STATS that had expected results."
  (+ (ert--stats-passed-expected stats)
     (ert--stats-failed-expected stats)))

(defun ert-stats-completed-unexpected (stats)
  "Return the number of tests in STATS that had unexpected results."
  (+ (ert--stats-passed-unexpected stats)
     (ert--stats-failed-unexpected stats)))

(defun ert-stats-completed (stats)
  "Number of tests in STATS that have run so far."
  (+ (ert-stats-completed-expected stats)
     (ert-stats-completed-unexpected stats)))

(defun ert-stats-total (stats)
  "Number of tests in STATS, regardless of whether they have run yet."
  (length (ert--stats-tests stats)))

;; The stats object of the current run, dynamically bound.  This is
;; used for the mode line progress indicator.
(defvar ert--current-run-stats nil)

(defun ert--stats-test-key (test)
  "Return the key used for TEST in the test map of ert--stats objects.

Returns the name of TEST if it has one, or TEST itself otherwise."
  (or (ert-test-name test) test))

(defun ert--stats-set-test-and-result (stats pos test result)
  "Change STATS by replacing the test at position POS with TEST and RESULT.

Also changes the counters in STATS to match."
  (let* ((tests (ert--stats-tests stats))
         (results (ert--stats-test-results stats))
         (old-test (aref tests pos))
         (map (ert--stats-test-map stats)))
    (flet ((update (d)
             (if (ert-test-result-expected-p (aref tests pos)
                                             (aref results pos))
                 (etypecase (aref results pos)
                   (ert-test-passed (incf (ert--stats-passed-expected stats) d))
                   (ert-test-failed (incf (ert--stats-failed-expected stats) d))
                   (null)
                   (ert-test-aborted-with-non-local-exit)
                   (ert-test-quit))
               (etypecase (aref results pos)
                 (ert-test-passed (incf (ert--stats-passed-unexpected stats) d))
                 (ert-test-failed (incf (ert--stats-failed-unexpected stats) d))
                 (null)
                 (ert-test-aborted-with-non-local-exit)
                 (ert-test-quit)))))
      ;; Adjust counters to remove the result that is currently in stats.
      (update -1)
      ;; Put new test and result into stats.
      (setf (aref tests pos) test
            (aref results pos) result)
      (remhash (ert--stats-test-key old-test) map)
      (setf (gethash (ert--stats-test-key test) map) pos)
      ;; Adjust counters to match new result.
      (update +1)
      nil)))

(defun ert--make-stats (tests selector)
  "Create a new `ert--stats' object for running TESTS.

SELECTOR is the selector that was used to select TESTS."
  (setq tests (ert--coerce-to-vector tests))
  (let ((map (make-hash-table :size (length tests))))
    (loop for i from 0
          for test across tests
          for key = (ert--stats-test-key test) do
          (assert (not (gethash key map)))
          (setf (gethash key map) i))
    (make-ert--stats :selector selector
                     :tests tests
                     :test-map map
                     :test-results (make-vector (length tests) nil)
                     :test-start-times (make-vector (length tests) nil)
                     :test-end-times (make-vector (length tests) nil))))

(defun ert-run-or-rerun-test (stats test listener)
  ;; checkdoc-order: nil
  "Run the single test TEST and record the result using STATS and LISTENER."
  (let ((ert--current-run-stats stats)
        (pos (ert--stats-test-pos stats test)))
    (ert--stats-set-test-and-result stats pos test nil)
    ;; Call listener after setting/before resetting
    ;; (ert--stats-current-test stats); the listener might refresh the
    ;; mode line display, and if the value is not set yet/any more
    ;; during this refresh, the mode line will flicker unnecessarily.
    (setf (ert--stats-current-test stats) test)
    (funcall listener 'test-started stats test)
    (setf (ert-test-most-recent-result test) nil)
    (setf (aref (ert--stats-test-start-times stats) pos) (current-time))
    (unwind-protect
        (ert-run-test test)
      (setf (aref (ert--stats-test-end-times stats) pos) (current-time))
      (let ((result (ert-test-most-recent-result test)))
        (ert--stats-set-test-and-result stats pos test result)
        (funcall listener 'test-ended stats test result))
      (setf (ert--stats-current-test stats) nil))))

(defun ert-run-tests (selector listener)
  "Run the tests specified by SELECTOR, sending progress updates to LISTENER."
  (let* ((tests (ert-select-tests selector t))
         (stats (ert--make-stats tests selector)))
    (setf (ert--stats-start-time stats) (current-time))
    (funcall listener 'run-started stats)
    (let ((abortedp t))
      (unwind-protect
          (let ((ert--current-run-stats stats))
            (force-mode-line-update)
            (unwind-protect
                (progn
                  (loop for test in tests do
                        (ert-run-or-rerun-test stats test listener))
                  (setq abortedp nil))
              (setf (ert--stats-aborted-p stats) abortedp)
              (setf (ert--stats-end-time stats) (current-time))
              (funcall listener 'run-ended stats abortedp)))
        (force-mode-line-update))
      stats)))

(defun ert--stats-test-pos (stats test)
  ;; checkdoc-order: nil
  "Return the position (index) of TEST in the run represented by STATS."
  (gethash (ert--stats-test-key test) (ert--stats-test-map stats)))


;;; Formatting functions shared across UIs.

(defun ert--format-time-iso8601 (time)
  "Format TIME in the variant of ISO 8601 used for timestamps in ERT."
  (format-time-string "%Y-%m-%d %T%z" time))

(defun ert-char-for-test-result (result expectedp)
  "Return a character that represents the test result RESULT.

EXPECTEDP specifies whether the result was expected."
  (let ((s (etypecase result
             (ert-test-passed ".P")
             (ert-test-failed "fF")
             (null "--")
             (ert-test-aborted-with-non-local-exit "aA")
             (ert-test-quit "qQ"))))
    (elt s (if expectedp 0 1))))

(defun ert-string-for-test-result (result expectedp)
  "Return a string that represents the test result RESULT.

EXPECTEDP specifies whether the result was expected."
  (let ((s (etypecase result
             (ert-test-passed '("passed" "PASSED"))
             (ert-test-failed '("failed" "FAILED"))
             (null '("unknown" "UNKNOWN"))
             (ert-test-aborted-with-non-local-exit '("aborted" "ABORTED"))
             (ert-test-quit '("quit" "QUIT")))))
    (elt s (if expectedp 0 1))))

(defun ert--pp-with-indentation-and-newline (object)
  "Pretty-print OBJECT, indenting it to the current column of point.
Ensures a final newline is inserted."
  (let ((begin (point)))
    (pp object (current-buffer))
    (unless (bolp) (insert "\n"))
    (save-excursion
      (goto-char begin)
      (indent-sexp))))

(defun ert--insert-infos (result)
  "Insert `ert-info' infos from RESULT into current buffer.

RESULT must be an `ert-test-result-with-condition'."
  (check-type result ert-test-result-with-condition)
  (dolist (info (ert-test-result-with-condition-infos result))
    (destructuring-bind (prefix . message) info
      (let ((begin (point))
            (indentation (make-string (+ (length prefix) 4) ?\s))
            (end nil))
        (unwind-protect
            (progn
              (insert message "\n")
              (setq end (copy-marker (point)))
              (goto-char begin)
              (insert "    " prefix)
              (forward-line 1)
              (while (< (point) end)
                (insert indentation)
                (forward-line 1)))
          (when end (set-marker end nil)))))))


;;; Running tests in batch mode.

(defvar ert-batch-backtrace-right-margin 70
  "*The maximum line length for printing backtraces in `ert-run-tests-batch'.")

;;;###autoload
(defun ert-run-tests-batch (&optional selector)
  "Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object."
  (unless selector (setq selector 't))
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (ecase event-type
       (run-started
        (destructuring-bind (stats) event-args
          (message "Running %s tests (%s)"
                   (length (ert--stats-tests stats))
                   (ert--format-time-iso8601 (ert--stats-start-time stats)))))
       (run-ended
        (destructuring-bind (stats abortedp) event-args
          (let ((unexpected (ert-stats-completed-unexpected stats))
                (expected-failures (ert--stats-failed-expected stats)))
            (message "\n%sRan %s tests, %s results as expected%s (%s)%s\n"
                     (if (not abortedp)
                         ""
                       "Aborted: ")
                     (ert-stats-total stats)
                     (ert-stats-completed-expected stats)
                     (if (zerop unexpected)
                         ""
                       (format ", %s unexpected" unexpected))
                     (ert--format-time-iso8601 (ert--stats-end-time stats))
                     (if (zerop expected-failures)
                         ""
                       (format "\n%s expected failures" expected-failures)))
            (unless (zerop unexpected)
              (message "%s unexpected results:" unexpected)
              (loop for test across (ert--stats-tests stats)
                    for result = (ert-test-most-recent-result test) do
                    (when (not (ert-test-result-expected-p test result))
                      (message "%9s  %S"
                               (ert-string-for-test-result result nil)
                               (ert-test-name test))))
              (message "%s" "")))))
       (test-started
        )
       (test-ended
        (destructuring-bind (stats test result) event-args
          (unless (ert-test-result-expected-p test result)
            (etypecase result
              (ert-test-passed
               (message "Test %S passed unexpectedly" (ert-test-name test)))
              (ert-test-result-with-condition
               (message "Test %S backtrace:" (ert-test-name test))
               (with-temp-buffer
                 (ert--print-backtrace (ert-test-result-with-condition-backtrace
                                        result))
                 (goto-char (point-min))
                 (while (not (eobp))
                   (let ((start (point))
                         (end (progn (end-of-line) (point))))
                     (setq end (min end
                                    (+ start ert-batch-backtrace-right-margin)))
                     (message "%s" (buffer-substring-no-properties
                                    start end)))
                   (forward-line 1)))
               (with-temp-buffer
                 (ert--insert-infos result)
                 (insert "    ")
                 (let ((print-escape-newlines t)
                       (print-level 5)
                       (print-length 10))
                   (ert--pp-with-indentation-and-newline
                    (ert-test-result-with-condition-condition result)))
                 (goto-char (1- (point-max)))
                 (assert (looking-at "\n"))
                 (delete-char 1)
                 (message "Test %S condition:" (ert-test-name test))
                 (message "%s" (buffer-string))))
              (ert-test-aborted-with-non-local-exit
               (message "Test %S aborted with non-local exit"
                        (ert-test-name test)))
              (ert-test-quit
               (message "Quit during %S" (ert-test-name test)))))
          (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                 (format-string (concat "%9s  %"
                                        (prin1-to-string (length max))
                                        "s/" max "  %S")))
            (message format-string
                     (ert-string-for-test-result result
                                                 (ert-test-result-expected-p
                                                  test result))
                     (1+ (ert--stats-test-pos stats test))
                     (ert-test-name test)))))))))

;;;###autoload
(defun ert-run-tests-batch-and-exit (&optional selector)
  "Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the tool detected an error outside
of the tests (e.g. invalid SELECTOR or bug in the code that runs
the tests)."
  (unwind-protect
      (let ((stats (ert-run-tests-batch selector)))
        (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace))
      (kill-emacs 2))))


;;; Utility functions for load/unload actions.

(defun ert--activate-font-lock-keywords ()
  "Activate font-lock keywords for some of ERT's symbols."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(defun* ert--remove-from-list (list-var element &key key test)
  "Remove ELEMENT from the value of LIST-VAR if present.

This can be used as an inverse of `add-to-list'."
  (unless key (setq key #'identity))
  (unless test (setq test #'equal))
  (setf (symbol-value list-var)
        (ert--remove* element
                      (symbol-value list-var)
                      :key key
                      :test test)))


;;; Some basic interactive functions.

(defun ert-read-test-name (prompt &optional default history
                                  add-default-to-prompt)
  "Read the name of a test and return it as a symbol.

Prompt with PROMPT.  If DEFAULT is a valid test name, use it as a
default.  HISTORY is the history to use; see `completing-read'.
If ADD-DEFAULT-TO-PROMPT is non-nil, PROMPT will be modified to
include the default, if any.

Signals an error if no test name was read."
  (etypecase default
    (string (let ((symbol (intern-soft default)))
              (unless (and symbol (ert-test-boundp symbol))
                (setq default nil))))
    (symbol (setq default
                  (if (ert-test-boundp default)
                      (symbol-name default)
                    nil)))
    (ert-test (setq default (ert-test-name default))))
  (when add-default-to-prompt
    (setq prompt (if (null default)
                     (format "%s: " prompt)
                   (format "%s (default %s): " prompt default))))
  (let ((input (completing-read prompt obarray #'ert-test-boundp
                                t nil history default nil)))
    ;; completing-read returns an empty string if default was nil and
    ;; the user just hit enter.
    (let ((sym (intern-soft input)))
      (if (ert-test-boundp sym)
          sym
        (error "Input does not name a test")))))

(defun ert-read-test-name-at-point (prompt)
  "Read the name of a test and return it as a symbol.
As a default, use the symbol at point, or the test at point if in
the ERT results buffer.  Prompt with PROMPT, augmented with the
default (if any)."
  (ert-read-test-name prompt (ert-test-at-point) nil t))

(defun ert-find-test-other-window (test-name)
  "Find, in another window, the definition of TEST-NAME."
  (interactive (list (ert-read-test-name-at-point "Find test definition: ")))
  (find-function-do-it test-name 'ert-deftest 'switch-to-buffer-other-window))

(defun ert-delete-test (test-name)
  "Make the test TEST-NAME unbound.

Nothing more than an interactive interface to `ert-make-test-unbound'."
  (interactive (list (ert-read-test-name-at-point "Delete test")))
  (ert-make-test-unbound test-name))

(defun ert-delete-all-tests ()
  "Make all symbols in `obarray' name no test."
  (interactive)
  (when (called-interactively-p 'any)
    (unless (y-or-n-p "Delete all tests? ")
      (error "Aborted")))
  ;; We can't use `ert-select-tests' here since that gives us only
  ;; test objects, and going from them back to the test name symbols
  ;; can fail if the `ert-test' defstruct has been redefined.
  (mapc #'ert-make-test-unbound (apropos-internal "" #'ert-test-boundp))
  t)


;;; Display of test progress and results.

;; An entry in the results buffer ewoc.  There is one entry per test.
(defstruct ert--ewoc-entry
  (test (assert nil))
  ;; If the result of this test was expected, its ewoc entry is hidden
  ;; initially.
  (hidden-p (assert nil))
  ;; An ewoc entry may be collapsed to hide details such as the error
  ;; condition.
  ;;
  ;; I'm not sure the ability to expand and collapse entries is still
  ;; a useful feature.
  (expanded-p t)
  ;; By default, the ewoc entry presents the error condition with
  ;; certain limits on how much to print (`print-level',
  ;; `print-length').  The user can interactively switch to a set of
  ;; higher limits.
  (extended-printer-limits-p nil))

;; Variables local to the results buffer.

;; The ewoc.
(defvar ert--results-ewoc)
;; The stats object.
(defvar ert--results-stats)
;; A string with one character per test.  Each character represents
;; the result of the corresponding test.  The string is displayed near
;; the top of the buffer and serves as a progress bar.
(defvar ert--results-progress-bar-string)
;; The position where the progress bar button begins.
(defvar ert--results-progress-bar-button-begin)
;; The test result listener that updates the buffer when tests are run.
(defvar ert--results-listener)

(defun ert-insert-test-name-button (test-name)
  "Insert a button that links to TEST-NAME."
  (insert-text-button (format "%S" test-name)
                      :type 'ert--test-name-button
                      'ert-test-name test-name))

(defun ert--results-format-expected-unexpected (expected unexpected)
  "Return a string indicating EXPECTED expected results, UNEXPECTED unexpected."
  (if (zerop unexpected)
      (format "%s" expected)
    (format "%s (%s unexpected)" (+ expected unexpected) unexpected)))

(defun ert--results-update-ewoc-hf (ewoc stats)
  "Update the header and footer of EWOC to show certain information from STATS.

Also sets `ert--results-progress-bar-button-begin'."
  (let ((run-count (ert-stats-completed stats))
        (results-buffer (current-buffer))
        ;; Need to save buffer-local value.
        (font-lock font-lock-mode))
    (ewoc-set-hf
     ewoc
     ;; header
     (with-temp-buffer
       (insert "Selector: ")
       (ert--insert-human-readable-selector (ert--stats-selector stats))
       (insert "\n")
       (insert
        (format (concat "Passed: %s\n"
                        "Failed: %s\n"
                        "Total:  %s/%s\n\n")
                (ert--results-format-expected-unexpected
                 (ert--stats-passed-expected stats)
                 (ert--stats-passed-unexpected stats))
                (ert--results-format-expected-unexpected
                 (ert--stats-failed-expected stats)
                 (ert--stats-failed-unexpected stats))
                run-count
                (ert-stats-total stats)))
       (insert
        (format "Started at:   %s\n"
                (ert--format-time-iso8601 (ert--stats-start-time stats))))
       ;; FIXME: This is ugly.  Need to properly define invariants of
       ;; the `stats' data structure.
       (let ((state (cond ((ert--stats-aborted-p stats) 'aborted)
                          ((ert--stats-current-test stats) 'running)
                          ((ert--stats-end-time stats) 'finished)
                          (t 'preparing))))
         (ecase state
           (preparing
            (insert ""))
           (aborted
            (cond ((ert--stats-current-test stats)
                   (insert "Aborted during test: ")
                   (ert-insert-test-name-button
                    (ert-test-name (ert--stats-current-test stats))))
                  (t
                   (insert "Aborted."))))
           (running
            (assert (ert--stats-current-test stats))
            (insert "Running test: ")
            (ert-insert-test-name-button (ert-test-name
                                          (ert--stats-current-test stats))))
           (finished
            (assert (not (ert--stats-current-test stats)))
            (insert "Finished.")))
         (insert "\n")
         (if (ert--stats-end-time stats)
             (insert
              (format "%s%s\n"
                      (if (ert--stats-aborted-p stats)
                          "Aborted at:   "
                        "Finished at:  ")
                      (ert--format-time-iso8601 (ert--stats-end-time stats))))
           (insert "\n"))
         (insert "\n"))
       (let ((progress-bar-string (with-current-buffer results-buffer
                                    ert--results-progress-bar-string)))
         (let ((progress-bar-button-begin
                (insert-text-button progress-bar-string
                                    :type 'ert--results-progress-bar-button
                                    'face (or (and font-lock
                                                   (ert-face-for-stats stats))
                                              'button))))
           ;; The header gets copied verbatim to the results buffer,
           ;; and all positions remain the same, so
           ;; `progress-bar-button-begin' will be the right position
           ;; even in the results buffer.
           (with-current-buffer results-buffer
             (set (make-local-variable 'ert--results-progress-bar-button-begin)
                  progress-bar-button-begin))))
       (insert "\n\n")
       (buffer-string))
     ;; footer
     ;;
     ;; We actually want an empty footer, but that would trigger a bug
     ;; in ewoc, sometimes clearing the entire buffer.  (It's possible
     ;; that this bug has been fixed since this has been tested; we
     ;; should test it again.)
     "\n")))


(defvar ert-test-run-redisplay-interval-secs .1
  "How many seconds ERT should wait between redisplays while running tests.

While running tests, ERT shows the current progress, and this variable
determines how frequently the progress display is updated.")

(defun ert--results-update-stats-display (ewoc stats)
  "Update EWOC and the mode line to show data from STATS."
  ;; TODO(ohler): investigate using `make-progress-reporter'.
  (ert--results-update-ewoc-hf ewoc stats)
  (force-mode-line-update)
  (redisplay t)
  (setf (ert--stats-next-redisplay stats)
        (+ (float-time) ert-test-run-redisplay-interval-secs)))

(defun ert--results-update-stats-display-maybe (ewoc stats)
  "Call `ert--results-update-stats-display' if not called recently.

EWOC and STATS are arguments for `ert--results-update-stats-display'."
  (when (>= (float-time) (ert--stats-next-redisplay stats))
    (ert--results-update-stats-display ewoc stats)))

(defun ert--tests-running-mode-line-indicator ()
  "Return a string for the mode line that shows the test run progress."
  (let* ((stats ert--current-run-stats)
         (tests-total (ert-stats-total stats))
         (tests-completed (ert-stats-completed stats)))
    (if (>= tests-completed tests-total)
        (format " ERT(%s/%s,finished)" tests-completed tests-total)
      (format " ERT(%s/%s):%s"
              (1+ tests-completed)
              tests-total
              (if (null (ert--stats-current-test stats))
                  "?"
                (format "%S"
                        (ert-test-name (ert--stats-current-test stats))))))))

(defun ert--make-xrefs-region (begin end)
  "Attach cross-references to function names between BEGIN and END.

BEGIN and END specify a region in the current buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      ;; Inhibit optimization in `debugger-make-xrefs' that would
      ;; sometimes insert unrelated backtrace info into our buffer.
      (let ((debugger-previous-backtrace nil))
        (debugger-make-xrefs)))))

(defun ert--string-first-line (s)
  "Return the first line of S, or S if it contains no newlines.

The return value does not include the line terminator."
  (substring s 0 (ert--string-position ?\n s)))

(defun ert-face-for-test-result (expectedp)
  "Return a face that shows whether a test result was expected or unexpected.

If EXPECTEDP is nil, returns the face for unexpected results; if
non-nil, returns the face for expected results.."
  (if expectedp 'ert-test-result-expected 'ert-test-result-unexpected))

(defun ert-face-for-stats (stats)
  "Return a face that represents STATS."
  (cond ((ert--stats-aborted-p stats) 'nil)
        ((plusp (ert-stats-completed-unexpected stats))
         (ert-face-for-test-result nil))
        ((eql (ert-stats-completed-expected stats) (ert-stats-total stats))
         (ert-face-for-test-result t))
        (t 'nil)))

(defun ert--print-test-for-ewoc (entry)
  "The ewoc print function for ewoc test entries.  ENTRY is the entry to print."
  (let* ((test (ert--ewoc-entry-test entry))
         (stats ert--results-stats)
         (result (let ((pos (ert--stats-test-pos stats test)))
                   (assert pos)
                   (aref (ert--stats-test-results stats) pos)))
         (hiddenp (ert--ewoc-entry-hidden-p entry))
         (expandedp (ert--ewoc-entry-expanded-p entry))
         (extended-printer-limits-p (ert--ewoc-entry-extended-printer-limits-p
                                     entry)))
    (cond (hiddenp)
          (t
           (let ((expectedp (ert-test-result-expected-p test result)))
             (insert-text-button (format "%c" (ert-char-for-test-result
                                               result expectedp))
                                 :type 'ert--results-expand-collapse-button
                                 'face (or (and font-lock-mode
                                                (ert-face-for-test-result
                                                 expectedp))
                                           'button)))
           (insert " ")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "\n")
           (when (and expandedp (not (eql result 'nil)))
             (when (ert-test-documentation test)
               (insert "    "
                       (propertize
                        (ert--string-first-line (ert-test-documentation test))
                        'font-lock-face 'font-lock-doc-face)
                       "\n"))
             (etypecase result
               (ert-test-passed
                (if (ert-test-result-expected-p test result)
                    (insert "    passed\n")
                  (insert "    passed unexpectedly\n"))
                (insert ""))
               (ert-test-result-with-condition
                (ert--insert-infos result)
                (let ((print-escape-newlines t)
                      (print-level (if extended-printer-limits-p 12 6))
                      (print-length (if extended-printer-limits-p 100 10)))
                  (insert "    ")
                  (let ((begin (point)))
                    (ert--pp-with-indentation-and-newline
                     (ert-test-result-with-condition-condition result))
                    (ert--make-xrefs-region begin (point)))))
               (ert-test-aborted-with-non-local-exit
                (insert "    aborted\n"))
               (ert-test-quit
                (insert "    quit\n")))
             (insert "\n")))))
  nil)

(defun ert--results-font-lock-function (enabledp)
  "Redraw the ERT results buffer after font-lock-mode was switched on or off.

ENABLEDP is true if font-lock-mode is switched on, false
otherwise."
  (ert--results-update-ewoc-hf ert--results-ewoc ert--results-stats)
  (ewoc-refresh ert--results-ewoc)
  (font-lock-default-function enabledp))

(defun ert--setup-results-buffer (stats listener buffer-name)
  "Set up a test results buffer.

STATS is the stats object; LISTENER is the results listener;
BUFFER-NAME, if non-nil, is the buffer name to use."
  (unless buffer-name (setq buffer-name "*ert*"))
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-results-mode)
        ;; Erase buffer again in case switching out of the previous
        ;; mode inserted anything.  (This happens e.g. when switching
        ;; from ert-results-mode to ert-results-mode when
        ;; font-lock-mode turns itself off in change-major-mode-hook.)
        (erase-buffer)
        (set (make-local-variable 'font-lock-function)
             'ert--results-font-lock-function)
        (let ((ewoc (ewoc-create 'ert--print-test-for-ewoc nil nil t)))
          (set (make-local-variable 'ert--results-ewoc) ewoc)
          (set (make-local-variable 'ert--results-stats) stats)
          (set (make-local-variable 'ert--results-progress-bar-string)
               (make-string (ert-stats-total stats)
                            (ert-char-for-test-result nil t)))
          (set (make-local-variable 'ert--results-listener) listener)
          (loop for test across (ert--stats-tests stats) do
                (ewoc-enter-last ewoc
                                 (make-ert--ewoc-entry :test test :hidden-p t)))
          (ert--results-update-ewoc-hf ert--results-ewoc ert--results-stats)
          (goto-char (1- (point-max)))
          buffer)))))


(defvar ert--selector-history nil
  "List of recent test selectors read from terminal.")

;; Should OUTPUT-BUFFER-NAME and MESSAGE-FN really be arguments here?
;; They are needed only for our automated self-tests at the moment.
;; Or should there be some other mechanism?
;;;###autoload
(defun ert-run-tests-interactively (selector
                                    &optional output-buffer-name message-fn)
  "Run the tests specified by SELECTOR and display the results in a buffer.

SELECTOR works as described in `ert-select-tests'.
OUTPUT-BUFFER-NAME and MESSAGE-FN should normally be nil; they
are used for automated self-tests and specify which buffer to use
and how to display message."
  (interactive
   (list (let ((default (if ert--selector-history
                            ;; Can't use `first' here as this form is
                            ;; not compiled, and `first' is not
                            ;; defined without cl.
                            (car ert--selector-history)
                          "t")))
           (read-from-minibuffer (if (null default)
                                     "Run tests: "
                                   (format "Run tests (default %s): " default))
                                 nil nil t 'ert--selector-history
                                 default nil))
         nil))
  (unless message-fn (setq message-fn 'message))
  (lexical-let ((output-buffer-name output-buffer-name)
                buffer
                listener
                (message-fn message-fn))
    (setq listener
          (lambda (event-type &rest event-args)
            (ecase event-type
              (run-started
               (destructuring-bind (stats) event-args
                 (setq buffer (ert--setup-results-buffer stats
                                                         listener
                                                         output-buffer-name))
                 (pop-to-buffer buffer)))
              (run-ended
               (destructuring-bind (stats abortedp) event-args
                 (funcall message-fn
                          "%sRan %s tests, %s results were as expected%s"
                          (if (not abortedp)
                              ""
                            "Aborted: ")
                          (ert-stats-total stats)
                          (ert-stats-completed-expected stats)
                          (let ((unexpected
                                 (ert-stats-completed-unexpected stats)))
                            (if (zerop unexpected)
                                ""
                              (format ", %s unexpected" unexpected))))
                 (ert--results-update-stats-display (with-current-buffer buffer
                                                      ert--results-ewoc)
                                                    stats)))
              (test-started
               (destructuring-bind (stats test) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert--results-ewoc)
                          (pos (ert--stats-test-pos stats test))
                          (node (ewoc-nth ewoc pos)))
                     (assert node)
                     (setf (ert--ewoc-entry-test (ewoc-data node)) test)
                     (aset ert--results-progress-bar-string pos
                           (ert-char-for-test-result nil t))
                     (ert--results-update-stats-display-maybe ewoc stats)
                     (ewoc-invalidate ewoc node)))))
              (test-ended
               (destructuring-bind (stats test result) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert--results-ewoc)
                          (pos (ert--stats-test-pos stats test))
                          (node (ewoc-nth ewoc pos)))
                     (when (ert--ewoc-entry-hidden-p (ewoc-data node))
                       (setf (ert--ewoc-entry-hidden-p (ewoc-data node))
                             (ert-test-result-expected-p test result)))
                     (aset ert--results-progress-bar-string pos
                           (ert-char-for-test-result result
                                                     (ert-test-result-expected-p
                                                      test result)))
                     (ert--results-update-stats-display-maybe ewoc stats)
                     (ewoc-invalidate ewoc node))))))))
    (ert-run-tests
     selector
     listener)))
;;;###autoload
(defalias 'ert 'ert-run-tests-interactively)


;;; Simple view mode for auxiliary information like stack traces or
;;; messages.  Mainly binds "q" for quit.

(define-derived-mode ert-simple-view-mode special-mode "ERT-View"
  "Major mode for viewing auxiliary information in ERT.")

;;; Commands and button actions for the results buffer.

(define-derived-mode ert-results-mode special-mode "ERT-Results"
  "Major mode for viewing results of ERT test runs.")

(loop for (key binding) in
      '(;; Stuff that's not in the menu.
        ("\t" forward-button)
        ([backtab] backward-button)
        ("j" ert-results-jump-between-summary-and-result)
        ("L" ert-results-toggle-printer-limits-for-test-at-point)
        ("n" ert-results-next-test)
        ("p" ert-results-previous-test)
        ;; Stuff that is in the menu.
        ("R" ert-results-rerun-all-tests)
        ("r" ert-results-rerun-test-at-point)
        ("d" ert-results-rerun-test-at-point-debugging-errors)
        ("." ert-results-find-test-at-point-other-window)
        ("b" ert-results-pop-to-backtrace-for-test-at-point)
        ("m" ert-results-pop-to-messages-for-test-at-point)
        ("l" ert-results-pop-to-should-forms-for-test-at-point)
        ("h" ert-results-describe-test-at-point)
        ("D" ert-delete-test)
        ("T" ert-results-pop-to-timings)
        )
      do
      (define-key ert-results-mode-map key binding))

(easy-menu-define ert-results-mode-menu ert-results-mode-map
  "Menu for `ert-results-mode'."
  '("ERT Results"
    ["Re-run all tests" ert-results-rerun-all-tests]
    "--"
    ["Re-run test" ert-results-rerun-test-at-point]
    ["Debug test" ert-results-rerun-test-at-point-debugging-errors]
    ["Show test definition" ert-results-find-test-at-point-other-window]
    "--"
    ["Show backtrace" ert-results-pop-to-backtrace-for-test-at-point]
    ["Show messages" ert-results-pop-to-messages-for-test-at-point]
    ["Show `should' forms" ert-results-pop-to-should-forms-for-test-at-point]
    ["Describe test" ert-results-describe-test-at-point]
    "--"
    ["Delete test" ert-delete-test]
    "--"
    ["Show execution time of each test" ert-results-pop-to-timings]
    ))

(define-button-type 'ert--results-progress-bar-button
  'action #'ert--results-progress-bar-button-action
  'help-echo "mouse-2, RET: Reveal test result")

(define-button-type 'ert--test-name-button
  'action #'ert--test-name-button-action
  'help-echo "mouse-2, RET: Find test definition")

(define-button-type 'ert--results-expand-collapse-button
  'action #'ert--results-expand-collapse-button-action
  'help-echo "mouse-2, RET: Expand/collapse test result")

(defun ert--results-test-node-or-null-at-point ()
  "If point is on a valid ewoc node, return it; return nil otherwise.

To be used in the ERT results buffer."
  (let* ((ewoc ert--results-ewoc)
         (node (ewoc-locate ewoc)))
    ;; `ewoc-locate' will return an arbitrary node when point is on
    ;; header or footer, or when all nodes are invisible.  So we need
    ;; to validate its return value here.
    ;;
    ;; Update: I'm seeing nil being returned in some cases now,
    ;; perhaps this has been changed?
    (if (and node
             (>= (point) (ewoc-location node))
             (not (ert--ewoc-entry-hidden-p (ewoc-data node))))
        node
      nil)))

(defun ert--results-test-node-at-point ()
  "If point is on a valid ewoc node, return it; signal an error otherwise.

To be used in the ERT results buffer."
  (or (ert--results-test-node-or-null-at-point)
      (error "No test at point")))

(defun ert-results-next-test ()
  "Move point to the next test.

To be used in the ERT results buffer."
  (interactive)
  (ert--results-move (ewoc-locate ert--results-ewoc) 'ewoc-next
                     "No tests below"))

(defun ert-results-previous-test ()
  "Move point to the previous test.

To be used in the ERT results buffer."
  (interactive)
  (ert--results-move (ewoc-locate ert--results-ewoc) 'ewoc-prev
                     "No tests above"))

(defun ert--results-move (node ewoc-fn error-message)
  "Move point from NODE to the previous or next node.

EWOC-FN specifies the direction and should be either `ewoc-prev'
or `ewoc-next'.  If there are no more nodes in that direction, an
error is signaled with the message ERROR-MESSAGE."
  (loop
   (setq node (funcall ewoc-fn ert--results-ewoc node))
   (when (null node)
     (error "%s" error-message))
   (unless (ert--ewoc-entry-hidden-p (ewoc-data node))
     (goto-char (ewoc-location node))
     (return))))

(defun ert--results-expand-collapse-button-action (button)
  "Expand or collapse the test node BUTTON belongs to."
  (let* ((ewoc ert--results-ewoc)
         (node (save-excursion
                 (goto-char (ert--button-action-position))
                 (ert--results-test-node-at-point)))
         (entry (ewoc-data node)))
    (setf (ert--ewoc-entry-expanded-p entry)
          (not (ert--ewoc-entry-expanded-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-find-test-at-point-other-window ()
  "Find the definition of the test at point in another window.

To be used in the ERT results buffer."
  (interactive)
  (let ((name (ert-test-at-point)))
    (unless name
      (error "No test at point"))
    (ert-find-test-other-window name)))

(defun ert--test-name-button-action (button)
  "Find the definition of the test BUTTON belongs to, in another window."
  (let ((name (button-get button 'ert-test-name)))
    (ert-find-test-other-window name)))

(defun ert--ewoc-position (ewoc node)
  ;; checkdoc-order: nil
  "Return the position of NODE in EWOC, or nil if NODE is not in EWOC."
  (loop for i from 0
        for node-here = (ewoc-nth ewoc 0) then (ewoc-next ewoc node-here)
        do (when (eql node node-here)
             (return i))
        finally (return nil)))

(defun ert-results-jump-between-summary-and-result ()
  "Jump back and forth between the test run summary and individual test results.

From an ewoc node, jumps to the character that represents the
same test in the progress bar, and vice versa.

To be used in the ERT results buffer."
  ;; Maybe this command isn't actually needed much, but if it is, it
  ;; seems like an indication that the UI design is not optimal.  If
  ;; jumping back and forth between a summary at the top of the buffer
  ;; and the error log in the remainder of the buffer is useful, then
  ;; the summary apparently needs to be easily accessible from the
  ;; error log, and perhaps it would be better to have it in a
  ;; separate buffer to keep it visible.
  (interactive)
  (let ((ewoc ert--results-ewoc)
        (progress-bar-begin ert--results-progress-bar-button-begin))
    (cond ((ert--results-test-node-or-null-at-point)
           (let* ((node (ert--results-test-node-at-point))
                  (pos (ert--ewoc-position ewoc node)))
             (goto-char (+ progress-bar-begin pos))))
          ((and (<= progress-bar-begin (point))
                (< (point) (button-end (button-at progress-bar-begin))))
           (let* ((node (ewoc-nth ewoc (- (point) progress-bar-begin)))
                  (entry (ewoc-data node)))
             (when (ert--ewoc-entry-hidden-p entry)
               (setf (ert--ewoc-entry-hidden-p entry) nil)
               (ewoc-invalidate ewoc node))
             (ewoc-goto-node ewoc node)))
          (t
           (goto-char progress-bar-begin)))))

(defun ert-test-at-point ()
  "Return the name of the test at point as a symbol, or nil if none."
  (or (and (eql major-mode 'ert-results-mode)
           (let ((test (ert--results-test-at-point-no-redefinition)))
             (and test (ert-test-name test))))
      (let* ((thing (thing-at-point 'symbol))
             (sym (intern-soft thing)))
        (and (ert-test-boundp sym)
             sym))))

(defun ert--results-test-at-point-no-redefinition ()
  "Return the test at point, or nil.

To be used in the ERT results buffer."
  (assert (eql major-mode 'ert-results-mode))
  (if (ert--results-test-node-or-null-at-point)
      (let* ((node (ert--results-test-node-at-point))
             (test (ert--ewoc-entry-test (ewoc-data node))))
        test)
    (let ((progress-bar-begin ert--results-progress-bar-button-begin))
      (when (and (<= progress-bar-begin (point))
                 (< (point) (button-end (button-at progress-bar-begin))))
        (let* ((test-index (- (point) progress-bar-begin))
               (test (aref (ert--stats-tests ert--results-stats)
                           test-index)))
          test)))))

(defun ert--results-test-at-point-allow-redefinition ()
  "Look up the test at point, and check whether it has been redefined.

To be used in the ERT results buffer.

Returns a list of two elements: the test (or nil) and a symbol
specifying whether the test has been redefined.

If a new test has been defined with the same name as the test at
point, replaces the test at point with the new test, and returns
the new test and the symbol `redefined'.

If the test has been deleted, returns the old test and the symbol
`deleted'.

If the test is still current, returns the test and the symbol nil.

If there is no test at point, returns a list with two nils."
  (let ((test (ert--results-test-at-point-no-redefinition)))
    (cond ((null test)
           `(nil nil))
          ((null (ert-test-name test))
           `(,test nil))
          (t
           (let* ((name (ert-test-name test))
                  (new-test (and (ert-test-boundp name)
                                 (ert-get-test name))))
             (cond ((eql test new-test)
                    `(,test nil))
                   ((null new-test)
                    `(,test deleted))
                   (t
                    (ert--results-update-after-test-redefinition
                     (ert--stats-test-pos ert--results-stats test)
                     new-test)
                    `(,new-test redefined))))))))

(defun ert--results-update-after-test-redefinition (pos new-test)
  "Update results buffer after the test at pos POS has been redefined.

Also updates the stats object.  NEW-TEST is the new test
definition."
  (let* ((stats ert--results-stats)
         (ewoc ert--results-ewoc)
         (node (ewoc-nth ewoc pos))
         (entry (ewoc-data node)))
    (ert--stats-set-test-and-result stats pos new-test nil)
    (setf (ert--ewoc-entry-test entry) new-test
          (aref ert--results-progress-bar-string pos) (ert-char-for-test-result
                                                       nil t))
    (ewoc-invalidate ewoc node))
  nil)

(defun ert--button-action-position ()
  "The buffer position where the last button action was triggered."
  (cond ((integerp last-command-event)
         (point))
        ((eventp last-command-event)
         (posn-point (event-start last-command-event)))
        (t (assert nil))))

(defun ert--results-progress-bar-button-action (button)
  "Jump to details for the test represented by the character clicked in BUTTON."
  (goto-char (ert--button-action-position))
  (ert-results-jump-between-summary-and-result))

(defun ert-results-rerun-all-tests ()
  "Re-run all tests, using the same selector.

To be used in the ERT results buffer."
  (interactive)
  (assert (eql major-mode 'ert-results-mode))
  (let ((selector (ert--stats-selector ert--results-stats)))
    (ert-run-tests-interactively selector (buffer-name))))

(defun ert-results-rerun-test-at-point ()
  "Re-run the test at point.

To be used in the ERT results buffer."
  (interactive)
  (destructuring-bind (test redefinition-state)
      (ert--results-test-at-point-allow-redefinition)
    (when (null test)
      (error "No test at point"))
    (let* ((stats ert--results-stats)
           (progress-message (format "Running %stest %S"
                                     (ecase redefinition-state
                                       ((nil) "")
                                       (redefined "new definition of ")
                                       (deleted "deleted "))
                                     (ert-test-name test))))
      ;; Need to save and restore point manually here: When point is on
      ;; the first visible ewoc entry while the header is updated, point
      ;; moves to the top of the buffer.  This is undesirable, and a
      ;; simple `save-excursion' doesn't prevent it.
      (let ((point (point)))
        (unwind-protect
            (unwind-protect
                (progn
                  (message "%s..." progress-message)
                  (ert-run-or-rerun-test stats test
                                         ert--results-listener))
              (ert--results-update-stats-display ert--results-ewoc stats)
              (message "%s...%s"
                       progress-message
                       (let ((result (ert-test-most-recent-result test)))
                         (ert-string-for-test-result
                          result (ert-test-result-expected-p test result)))))
          (goto-char point))))))

(defun ert-results-rerun-test-at-point-debugging-errors ()
  "Re-run the test at point with `ert-debug-on-error' bound to t.

To be used in the ERT results buffer."
  (interactive)
  (let ((ert-debug-on-error t))
    (ert-results-rerun-test-at-point)))

(defun ert-results-pop-to-backtrace-for-test-at-point ()
  "Display the backtrace for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((test (ert--results-test-at-point-no-redefinition))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (etypecase result
      (ert-test-passed (error "Test passed, no backtrace available"))
      (ert-test-result-with-condition
       (let ((backtrace (ert-test-result-with-condition-backtrace result))
             (buffer (get-buffer-create "*ERT Backtrace*")))
         (pop-to-buffer buffer)
         (let ((inhibit-read-only t))
           (buffer-disable-undo)
           (erase-buffer)
           (ert-simple-view-mode)
           ;; Use unibyte because `debugger-setup-buffer' also does so.
           (set-buffer-multibyte nil)
           (setq truncate-lines t)
           (ert--print-backtrace backtrace)
           (debugger-make-xrefs)
           (goto-char (point-min))
           (insert "Backtrace for test `")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "':\n")))))))

(defun ert-results-pop-to-messages-for-test-at-point ()
  "Display the part of the *Messages* buffer generated during the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((test (ert--results-test-at-point-no-redefinition))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (let ((buffer (get-buffer-create "*ERT Messages*")))
      (pop-to-buffer buffer)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-simple-view-mode)
        (insert (ert-test-result-messages result))
        (goto-char (point-min))
        (insert "Messages for test `")
        (ert-insert-test-name-button (ert-test-name test))
        (insert "':\n")))))

(defun ert-results-pop-to-should-forms-for-test-at-point ()
  "Display the list of `should' forms executed during the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((test (ert--results-test-at-point-no-redefinition))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (let ((buffer (get-buffer-create "*ERT list of should forms*")))
      (pop-to-buffer buffer)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-simple-view-mode)
        (if (null (ert-test-result-should-forms result))
            (insert "\n(No should forms during this test.)\n")
          (loop for form-description in (ert-test-result-should-forms result)
                for i from 1 do
                (insert "\n")
                (insert (format "%s: " i))
                (let ((begin (point)))
                  (ert--pp-with-indentation-and-newline form-description)
                  (ert--make-xrefs-region begin (point)))))
        (goto-char (point-min))
        (insert "`should' forms executed during test `")
        (ert-insert-test-name-button (ert-test-name test))
        (insert "':\n")
        (insert "\n")
        (insert (concat "(Values are shallow copies and may have "
                        "looked different during the test if they\n"
                        "have been modified destructively.)\n"))
        (forward-line 1)))))

(defun ert-results-toggle-printer-limits-for-test-at-point ()
  "Toggle how much of the condition to print for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((ewoc ert--results-ewoc)
         (node (ert--results-test-node-at-point))
         (entry (ewoc-data node)))
    (setf (ert--ewoc-entry-extended-printer-limits-p entry)
          (not (ert--ewoc-entry-extended-printer-limits-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-pop-to-timings ()
  "Display test timings for the last run.

To be used in the ERT results buffer."
  (interactive)
  (let* ((stats ert--results-stats)
         (start-times (ert--stats-test-start-times stats))
         (end-times (ert--stats-test-end-times stats))
         (buffer (get-buffer-create "*ERT timings*"))
         (data (loop for test across (ert--stats-tests stats)
                     for start-time across (ert--stats-test-start-times stats)
                     for end-time across (ert--stats-test-end-times stats)
                     collect (list test
                                   (float-time (subtract-time end-time
                                                              start-time))))))
    (setq data (sort data (lambda (a b)
                            (> (second a) (second b)))))
    (pop-to-buffer buffer)
    (let ((inhibit-read-only t))
      (buffer-disable-undo)
      (erase-buffer)
      (ert-simple-view-mode)
      (if (null data)
          (insert "(No data)\n")
        (insert (format "%-3s  %8s %8s\n" "" "time" "cumul"))
        (loop for (test time) in data
              for cumul-time = time then (+ cumul-time time)
              for i from 1 do
              (let ((begin (point)))
                (insert (format "%3s: %8.3f %8.3f " i time cumul-time))
                (ert-insert-test-name-button (ert-test-name test))
                (insert "\n"))))
      (goto-char (point-min))
      (insert "Tests by run time (seconds):\n\n")
      (forward-line 1))))

;;;###autoload
(defun ert-describe-test (test-or-test-name)
  "Display the documentation for TEST-OR-TEST-NAME (a symbol or ert-test)."
  (interactive (list (ert-read-test-name-at-point "Describe test")))
  (when (< emacs-major-version 24)
    (error "Requires Emacs 24"))
  (let (test-name
        test-definition)
    (etypecase test-or-test-name
      (symbol (setq test-name test-or-test-name
                    test-definition (ert-get-test test-or-test-name)))
      (ert-test (setq test-name (ert-test-name test-or-test-name)
                      test-definition test-or-test-name)))
    (help-setup-xref (list #'ert-describe-test test-or-test-name)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (insert (if test-name (format "%S" test-name) "<anonymous test>"))
          (insert " is a test")
          (let ((file-name (and test-name
                                (symbol-file test-name 'ert-deftest))))
            (when file-name
              (insert " defined in `" (file-name-nondirectory file-name) "'")
              (save-excursion
                (re-search-backward "`\\([^`']+\\)'" nil t)
                (help-xref-button 1 'help-function-def test-name file-name)))
            (insert ".")
            (fill-region-as-paragraph (point-min) (point))
            (insert "\n\n")
            (unless (and (ert-test-boundp test-name)
                         (eql (ert-get-test test-name) test-definition))
              (let ((begin (point)))
                (insert "Note: This test has been redefined or deleted, "
                        "this documentation refers to an old definition.")
                (fill-region-as-paragraph begin (point)))
              (insert "\n\n"))
            (insert (or (ert-test-documentation test-definition)
                        "It is not documented.")
                    "\n")))))))

(defun ert-results-describe-test-at-point ()
  "Display the documentation of the test at point.

To be used in the ERT results buffer."
  (interactive)
  (ert-describe-test (ert--results-test-at-point-no-redefinition)))


;;; Actions on load/unload.

(add-to-list 'find-function-regexp-alist '(ert-deftest . ert--find-test-regexp))
(add-to-list 'minor-mode-alist '(ert--current-run-stats
                                 (:eval
                                  (ert--tests-running-mode-line-indicator))))
(add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)

(defun ert--unload-function ()
  "Unload function to undo the side-effects of loading ert.el."
  (ert--remove-from-list 'find-function-regexp-alist 'ert-deftest :key #'car)
  (ert--remove-from-list 'minor-mode-alist 'ert--current-run-stats :key #'car)
  (ert--remove-from-list 'emacs-lisp-mode-hook
                         'ert--activate-font-lock-keywords)
  nil)

(defvar ert-unload-hook '())
(add-hook 'ert-unload-hook 'ert--unload-function)


(provide 'ert)

;;; ert.el ends here
