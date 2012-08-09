;;; edebug.el --- a source-level debugger for Emacs Lisp

;; Copyright (C) 1988-1995, 1997, 1999-2012 Free Software Foundation, Inc.

;; Author: Daniel LaLiberte <liberte@holonexus.org>
;; Maintainer: FSF
;; Keywords: lisp, tools, maint

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

;; This minor mode allows programmers to step through Emacs Lisp
;; source code while executing functions.  You can also set
;; breakpoints, trace (stopping at each expression), evaluate
;; expressions as if outside Edebug, reevaluate and display a list of
;; expressions, trap errors normally caught by debug, and display a
;; debug style backtrace.

;;; Minimal Instructions
;; =====================

;; First evaluate a defun with C-M-x, then run the function.  Step
;; through the code with SPC, mark breakpoints with b, go until a
;; breakpoint is reached with g, and quit execution with q.  Use the
;; "?" command in edebug to describe other commands.
;; See the Emacs Lisp Reference Manual for more details.

;; If you wish to change the default edebug global command prefix, change:
;; (setq edebug-global-prefix "\C-xX")

;; Edebug was written by
;; Daniel LaLiberte
;; GTE Labs
;; 40 Sylvan Rd
;; Waltham, MA  02254
;; liberte@holonexus.org

;;; Code:

;;; Bug reporting

(defalias 'edebug-submit-bug-report 'report-emacs-bug)

;;; Options

(defgroup edebug nil
  "A source-level debugger for Emacs Lisp."
  :group 'lisp)


(defcustom edebug-setup-hook nil
  "Functions to call before edebug is used.
Each time it is set to a new value, Edebug will call those functions
once and then reset `edebug-setup-hook' to nil.  You could use this
to load up Edebug specifications associated with a package you are
using, but only when you also use Edebug."
  :type 'hook
  :group 'edebug)

;; edebug-all-defs and edebug-all-forms need to be autoloaded
;; because the byte compiler binds them; as a result, if edebug
;; is first loaded for a require in a compilation, they will be left unbound.

;;;###autoload
(defcustom edebug-all-defs nil
  "If non-nil, evaluating defining forms instruments for Edebug.
This applies to `eval-defun', `eval-region', `eval-buffer', and
`eval-current-buffer'.  `eval-region' is also called by
`eval-last-sexp', and `eval-print-last-sexp'.

You can use the command `edebug-all-defs' to toggle the value of this
variable.  You may wish to make it local to each buffer with
\(make-local-variable 'edebug-all-defs) in your
`emacs-lisp-mode-hook'."
  :type 'boolean
  :group 'edebug)

;; edebug-all-defs and edebug-all-forms need to be autoloaded
;; because the byte compiler binds them; as a result, if edebug
;; is first loaded for a require in a compilation, they will be left unbound.

;;;###autoload
(defcustom edebug-all-forms nil
  "Non-nil means evaluation of all forms will instrument for Edebug.
This doesn't apply to loading or evaluations in the minibuffer.
Use the command `edebug-all-forms' to toggle the value of this option."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-eval-macro-args nil
  "Non-nil means all macro call arguments may be evaluated.
If this variable is nil, the default, Edebug will *not* wrap
macro call arguments as if they will be evaluated.
For each macro, an `edebug-form-spec' overrides this option.
So to specify exceptions for macros that have some arguments evaluated
and some not, use `def-edebug-spec' to specify an `edebug-form-spec'."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-save-windows t
  "If non-nil, Edebug saves and restores the window configuration.
That takes some time, so if your program does not care what happens to
the window configurations, it is better to set this variable to nil.

If the value is a list, only the listed windows are saved and
restored.

`edebug-toggle-save-windows' may be used to change this variable."
  :type '(choice boolean (repeat string))
  :group 'edebug)

(defcustom edebug-save-displayed-buffer-points nil
  "If non-nil, save and restore point in all displayed buffers.

Saving and restoring point in other buffers is necessary if you are
debugging code that changes the point of a buffer that is displayed
in a non-selected window.  If Edebug or the user then selects the
window, the buffer's point will be changed to the window's point.

Saving and restoring point in all buffers is expensive, since it
requires selecting each window twice, so enable this only if you
need it."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-initial-mode 'step
  "Initial execution mode for Edebug, if non-nil.
If this variable is non-nil, it specifies the initial execution mode
for Edebug when it is first activated.  Possible values are step, next,
go, Go-nonstop, trace, Trace-fast, continue, and Continue-fast."
  :type '(choice (const step) (const next) (const go)
		 (const Go-nonstop) (const trace)
		 (const Trace-fast) (const continue)
		 (const Continue-fast))
  :group 'edebug)

(defcustom edebug-trace nil
  "Non-nil means display a trace of function entry and exit.
Tracing output is displayed in a buffer named `*edebug-trace*', one
function entry or exit per line, indented by the recursion level.

You can customize by replacing functions `edebug-print-trace-before'
and `edebug-print-trace-after'."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-test-coverage nil
  "If non-nil, Edebug tests coverage of all expressions debugged.
This is done by comparing the result of each expression with the
previous result.  Coverage is considered OK if two different
results are found.

Use `edebug-display-freq-count' to display the frequency count and
coverage information for a definition."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-continue-kbd-macro nil
  "If non-nil, continue defining or executing any keyboard macro.
Use this with caution since it is not debugged."
  :type 'boolean
  :group 'edebug)


(defcustom edebug-print-length 50
  "If non-nil, default value of `print-length' for printing results in Edebug."
  :type 'integer
  :group 'edebug)
(defcustom edebug-print-level 50
  "If non-nil, default value of `print-level' for printing results in Edebug."
  :type 'integer
  :group 'edebug)
(defcustom edebug-print-circle t
  "If non-nil, default value of `print-circle' for printing results in Edebug."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-unwrap-results nil
  "Non-nil if Edebug should unwrap results of expressions.
That is, Edebug will try to remove its own instrumentation from the result.
This is useful when debugging macros where the results of expressions
are instrumented expressions.  But don't do this when results might be
circular or an infinite loop will result."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-on-error t
  "Value bound to `debug-on-error' while Edebug is active.

If `debug-on-error' is non-nil, that value is still used.

If the value is a list of signal names, Edebug will stop when any of
these errors are signaled from Lisp code whether or not the signal is
handled by a `condition-case'.  This option is useful for debugging
signals that *are* handled since they would otherwise be missed.
After execution is resumed, the error is signaled again."
  :type '(choice (const :tag "off")
		 (repeat :menu-tag "When"
			 :value (nil)
			 (symbol :format "%v"))
		 (const :tag "always" t))
  :group 'edebug)

(defcustom edebug-on-quit t
  "Value bound to `debug-on-quit' while Edebug is active."
  :type 'boolean
  :group 'edebug)

(defcustom edebug-global-break-condition nil
  "If non-nil, an expression to test for at every stop point.
If the result is non-nil, then break.  Errors are ignored."
  :type 'sexp
  :group 'edebug)

(defcustom edebug-sit-for-seconds 1
  "Number of seconds to pause when execution mode is `trace' or `continue'."
  :type 'number
  :group 'edebug)

;;; Form spec utilities.

(defmacro def-edebug-form-spec (symbol spec-form)
  "For compatibility with old version."
  (def-edebug-spec symbol (eval spec-form)))
(make-obsolete 'def-edebug-form-spec 'def-edebug-spec "22.1")

(defun get-edebug-spec (symbol)
  ;; Get the spec of symbol resolving all indirection.
  (let ((edebug-form-spec (get symbol 'edebug-form-spec))
	indirect)
    (while (and (symbolp edebug-form-spec)
		(setq indirect (get edebug-form-spec 'edebug-form-spec)))
      ;; (edebug-trace "indirection: %s" edebug-form-spec)
      (setq edebug-form-spec indirect))
    edebug-form-spec
    ))

;;;###autoload
(defun edebug-basic-spec (spec)
  "Return t if SPEC uses only extant spec symbols.
An extant spec symbol is a symbol that is not a function and has a
`edebug-form-spec' property."
  (cond ((listp spec)
	 (catch 'basic
	   (while spec
	     (unless (edebug-basic-spec (car spec)) (throw 'basic nil))
	     (setq spec (cdr spec)))
	   t))
	((symbolp spec)
	 (unless (functionp spec) (get spec 'edebug-form-spec)))))

;;; Utilities

;; Define edebug-gensym - from old cl.el
(defvar edebug-gensym-index 0
  "Integer used by `edebug-gensym' to produce new names.")

(defun edebug-gensym (&optional prefix)
  "Generate a fresh uninterned symbol.
There is an optional argument, PREFIX.  PREFIX is the string
that begins the new name.  Most people take just the default,
except when debugging needs suggest otherwise."
  (if (null prefix)
      (setq prefix "G"))
  (let ((newsymbol nil)
        (newname   ""))
    (while (not newsymbol)
      (setq newname (concat prefix (int-to-string edebug-gensym-index)))
      (setq edebug-gensym-index (+ edebug-gensym-index 1))
      (if (not (intern-soft newname))
          (setq newsymbol (make-symbol newname))))
    newsymbol))

(defun edebug-lambda-list-keywordp (object)
  "Return t if OBJECT is a lambda list keyword.
A lambda list keyword is a symbol that starts with `&'."
  (and (symbolp object)
       (= ?& (aref (symbol-name object) 0))))


(defun edebug-last-sexp ()
  ;; Return the last sexp before point in current buffer.
  ;; Assumes Emacs Lisp syntax is active.
  (car
   (read-from-string
    (buffer-substring
     (save-excursion
       (forward-sexp -1)
       (point))
     (point)))))

(defun edebug-window-list ()
  "Return a list of windows, in order of `next-window'."
  ;; This doesn't work for epoch.
  (let (window-list)
    (walk-windows (lambda (w) (push w window-list)))
    (nreverse window-list)))

;; Not used.
'(defun edebug-two-window-p ()
  "Return t if there are two windows."
  (and (not (one-window-p))
       (eq (selected-window)
	   (next-window (next-window (selected-window))))))

(defsubst edebug-lookup-function (object)
  (while (and (symbolp object) (fboundp object))
    (setq object (symbol-function object)))
  object)

(defun edebug-macrop (object)
  "Return the macro named by OBJECT, or nil if it is not a macro."
  (setq object (edebug-lookup-function object))
  (if (and (listp object)
	   (eq 'macro (car object))
	   (functionp (cdr object)))
      object))

(defun edebug-sort-alist (alist function)
  ;; Return the ALIST sorted with comparison function FUNCTION.
  ;; This uses 'sort so the sorting is destructive.
  (sort alist (function
	       (lambda (e1 e2)
		 (funcall function (car e1) (car e2))))))

;;(def-edebug-spec edebug-save-restriction t)

;; Not used.  If it is used, def-edebug-spec must be defined before use.
'(defmacro edebug-save-restriction (&rest body)
  "Evaluate BODY while saving the current buffers restriction.
BODY may change buffer outside of current restriction, unlike
save-restriction.  BODY may change the current buffer,
and the restriction will be restored to the original buffer,
and the current buffer remains current.
Return the result of the last expression in BODY."
  `(let ((edebug:s-r-beg (point-min-marker))
	 (edebug:s-r-end (point-max-marker)))
     (unwind-protect
	 (progn ,@body)
       (with-current-buffer (marker-buffer edebug:s-r-beg)
	 (narrow-to-region edebug:s-r-beg edebug:s-r-end)))))

;;; Display

(defconst edebug-trace-buffer "*edebug-trace*"
  "Name of the buffer to put trace info in.")

(defun edebug-pop-to-buffer (buffer &optional window)
  ;; Like pop-to-buffer, but select window where BUFFER was last shown.
  ;; Select WINDOW if it is provided and still exists.  Otherwise,
  ;; if buffer is currently shown in several windows, choose one.
  ;; Otherwise, find a new window, possibly splitting one.
  (setq window
	(cond
	 ((and (edebug-window-live-p window)
	       (eq (window-buffer window) buffer))
	  window)
	 ((eq (window-buffer (selected-window)) buffer)
	  ;; Selected window already displays BUFFER.
	  (selected-window))
	 ((edebug-get-buffer-window buffer))
	 ((one-window-p 'nomini)
	  ;; When there's one window only, split it.
	  (split-window))
	 ((let ((trace-window (get-buffer-window edebug-trace-buffer)))
	    (catch 'found
	      (dolist (elt (window-list nil 'nomini))
		(unless (or (eq elt (selected-window)) (eq elt trace-window)
			    (window-dedicated-p elt))
		  ;; Found a non-dedicated window not showing
		  ;; `edebug-trace-buffer', use it.
		  (throw 'found elt))))))
	 ;; All windows are dedicated or show `edebug-trace-buffer', split
	 ;; selected one.
	 (t (split-window))))
  (select-window window)
  (set-window-buffer window buffer)
  (set-window-hscroll window 0);; should this be??
  ;; Selecting the window does not set the buffer until command loop.
  ;;(set-buffer buffer)
  )

(defun edebug-get-displayed-buffer-points ()
  ;; Return a list of buffer point pairs, for all displayed buffers.
  (let (list)
    (walk-windows (lambda (w)
		    (unless (eq w (selected-window))
		      (push (cons (window-buffer w)
				  (window-point w))
			    list))))
    list))


(defun edebug-set-buffer-points (buffer-points)
  ;; Restore the buffer-points created by edebug-get-displayed-buffer-points.
  (save-current-buffer
    (mapcar (lambda (buf-point)
	      (when (buffer-live-p (car buf-point))
		(set-buffer (car buf-point))
		(goto-char (cdr buf-point))))
	    buffer-points)))

(defun edebug-current-windows (which-windows)
  ;; Get either a full window configuration or some window information.
  (if (listp which-windows)
      (mapcar (function (lambda (window)
			  (if (edebug-window-live-p window)
			      (list window
				    (window-buffer window)
				    (window-point window)
				    (window-start window)
				    (window-hscroll window)))))
	      which-windows)
    (current-window-configuration)))

(defun edebug-set-windows (window-info)
  ;; Set either a full window configuration or some window information.
  (if (listp window-info)
      (mapcar (function
	       (lambda (one-window-info)
		 (if one-window-info
		     (apply (function
			     (lambda (window buffer point start hscroll)
			       (if (edebug-window-live-p window)
				   (progn
				     (set-window-buffer window buffer)
				     (set-window-point window point)
				     (set-window-start window start)
				     (set-window-hscroll window hscroll)))))
			    one-window-info))))
	      window-info)
    (set-window-configuration window-info)))

(defalias 'edebug-get-buffer-window 'get-buffer-window)
(defalias 'edebug-sit-for 'sit-for)
(defalias 'edebug-input-pending-p 'input-pending-p)


;;; Redefine read and eval functions
;; read is redefined to maybe instrument forms.
;; eval-defun is redefined to check edebug-all-forms and edebug-all-defs.

;; Save the original read function
(or (fboundp 'edebug-original-read)
    (defalias 'edebug-original-read  (symbol-function 'read)))

(defun edebug-read (&optional stream)
  "Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of `standard-input' (which see).
STREAM or the value of `standard-input' may be:
 a buffer (read from point and advance it)
 a marker (read from where it points and advance it)
 a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
 a string (takes text from string, starting at the beginning)
 t (read text line using minibuffer and use it).

This version, from Edebug, maybe instruments the expression.  But the
STREAM must be the current buffer to do so.  Whether it instruments is
also dependent on the values of `edebug-all-defs' and
`edebug-all-forms'."
  (or stream (setq stream standard-input))
  (if (eq stream (current-buffer))
      (edebug-read-and-maybe-wrap-form)
    (edebug-original-read stream)))

(or (fboundp 'edebug-original-eval-defun)
    (defalias 'edebug-original-eval-defun (symbol-function 'eval-defun)))

;; We should somehow arrange to be able to do this
;; without actually replacing the eval-defun command.
(defun edebug-eval-defun (edebug-it)
  "Evaluate the top-level form containing point, or after point.

If the current defun is actually a call to `defvar', then reset the
variable using its initial value expression even if the variable
already has some other value.  (Normally `defvar' does not change the
variable's value if it already has a value.)  Treat `defcustom'
similarly.  Reinitialize the face according to `defface' specification.

With a prefix argument, instrument the code for Edebug.

Setting `edebug-all-defs' to a non-nil value reverses the meaning of
the prefix argument.  Code is then instrumented when this function is
invoked without a prefix argument

If acting on a `defun' for FUNCTION, and the function was instrumented,
`Edebug: FUNCTION' is printed in the minibuffer.  If not instrumented,
just FUNCTION is printed.

If not acting on a `defun', the result of evaluation is displayed in
the minibuffer."
  (interactive "P")
  (let* ((edebugging (not (eq (not edebug-it) (not edebug-all-defs))))
	 (edebug-result)
	 (form
	  (let ((edebug-all-forms edebugging)
		(edebug-all-defs (eq edebug-all-defs (not edebug-it))))
	    (edebug-read-top-level-form))))
    ;; This should be consistent with `eval-defun-1', but not the
    ;; same, since that gets a macroexpanded form.
    (cond ((and (eq (car form) 'defvar)
		(cdr-safe (cdr-safe form)))
	   ;; Force variable to be bound.
	   (makunbound (nth 1 form)))
	  ((and (eq (car form) 'defcustom)
		(default-boundp (nth 1 form)))
	   ;; Force variable to be bound.
           ;; FIXME: Shouldn't this use the :setter or :initializer?
	   (set-default (nth 1 form) (eval (nth 2 form) lexical-binding)))
          ((eq (car form) 'defface)
           ;; Reset the face.
           (setq face-new-frame-defaults
                 (assq-delete-all (nth 1 form) face-new-frame-defaults))
           (put (nth 1 form) 'face-defface-spec nil)
	   ;; See comments in `eval-defun-1' for purpose of code below
	   (setq form (prog1 `(prog1 ,form
				(put ',(nth 1 form) 'saved-face
				     ',(get (nth 1 form) 'saved-face))
				(put ',(nth 1 form) 'customized-face
				     ,(nth 2 form)))
			(put (nth 1 form) 'saved-face nil)))))
    (setq edebug-result (eval (eval-sexp-add-defvars form) lexical-binding))
    (if (not edebugging)
	(princ edebug-result)
      edebug-result)))


;;;###autoload
(defalias 'edebug-defun 'edebug-eval-top-level-form)

;;;###autoload
(defun edebug-eval-top-level-form ()
  "Evaluate the top level form point is in, stepping through with Edebug.
This is like `eval-defun' except that it steps the code for Edebug
before evaluating it.  It displays the value in the echo area
using `eval-expression' (which see).

If you do this on a function definition such as a defun or defmacro,
it defines the function and instruments its definition for Edebug,
so it will do Edebug stepping when called later.  It displays
`Edebug: FUNCTION' in the echo area to indicate that FUNCTION is now
instrumented for Edebug.

If the current defun is actually a call to `defvar' or `defcustom',
evaluating it this way resets the variable using its initial value
expression even if the variable already has some other value.
\(Normally `defvar' and `defcustom' do not alter the value if there
already is one.)"
  (interactive)
  (eval-expression
   ;; Bind edebug-all-forms only while reading, not while evalling
   ;; but this causes problems while edebugging edebug.
   (let ((edebug-all-forms t)
	 (edebug-all-defs t))
     (eval-sexp-add-defvars
      (edebug-read-top-level-form)))))


(defun edebug-read-top-level-form ()
  (let ((starting-point (point)))
    (end-of-defun)
    (beginning-of-defun)
    (prog1
	(edebug-read-and-maybe-wrap-form)
      ;; Recover point, but only if no error occurred.
      (goto-char starting-point))))


;; Compatibility with old versions.
(defalias 'edebug-all-defuns 'edebug-all-defs)

;;;###autoload
(defun edebug-all-defs ()
  "Toggle edebugging of all definitions."
  (interactive)
  (setq edebug-all-defs (not edebug-all-defs))
  (message "Edebugging all definitions is %s."
	   (if edebug-all-defs "on" "off")))


;;;###autoload
(defun edebug-all-forms ()
  "Toggle edebugging of all forms."
  (interactive)
  (setq edebug-all-forms (not edebug-all-forms))
  (message "Edebugging all forms is %s."
	   (if edebug-all-forms "on" "off")))


(defun edebug-install-read-eval-functions ()
  (interactive)
  ;; Don't install if already installed.
  (unless load-read-function
    (setq load-read-function 'edebug-read)
    (defalias 'eval-defun 'edebug-eval-defun)))

(defun edebug-uninstall-read-eval-functions ()
  (interactive)
  (setq load-read-function nil)
  (defalias 'eval-defun (symbol-function 'edebug-original-eval-defun)))


;;; Edebug internal data

;; The internal data that is needed for edebugging is kept in the
;; buffer-local variable `edebug-form-data'.

(make-variable-buffer-local 'edebug-form-data)

(defvar edebug-form-data nil)
;; A list of entries associating symbols with buffer regions.
;; This is an automatic buffer local variable.  Each entry looks like:
;; @code{(@var{symbol} @var{begin-marker} @var{end-marker}).  The markers
;; are at the beginning and end of an entry level form and @var{symbol} is
;; a symbol that holds all edebug related information for the form on its
;; property list.

;; In the future, the symbol will be irrelevant and edebug data will
;; be stored in the definitions themselves rather than in the property
;; list of a symbol.

(defun edebug-make-form-data-entry (symbol begin end)
  (list symbol begin end))

(defsubst edebug-form-data-name (entry)
  (car entry))

(defsubst edebug-form-data-begin (entry)
  (nth 1 entry))

(defsubst edebug-form-data-end (entry)
  (nth 2 entry))

(defsubst edebug-set-form-data-entry (entry name begin end)
  (setcar entry name);; in case name is changed
  (set-marker (nth 1 entry) begin)
  (set-marker (nth 2 entry) end))

(defun edebug-get-form-data-entry (pnt &optional end-point)
  ;; Find the edebug form data entry which is closest to PNT.
  ;; If END-POINT is supplied, match must be exact.
  ;; Return `nil' if none found.
  (let ((rest edebug-form-data)
	closest-entry
	(closest-dist 999999))  ;; need maxint here
    (while (and rest (< 0 closest-dist))
      (let* ((entry (car rest))
	     (begin (edebug-form-data-begin entry))
	     (dist (- pnt begin)))
	(setq rest (cdr rest))
	(if (and (<= 0 dist)
		 (< dist closest-dist)
		 (or (not end-point)
		     (= end-point (edebug-form-data-end entry)))
		 (<= pnt (edebug-form-data-end entry)))
	    (setq closest-dist dist
		  closest-entry entry))))
    closest-entry))

;; Also need to find all contained entries,
;; and find an entry given a symbol, which should be just assq.

(defun edebug-form-data-symbol ()
;; Return the edebug data symbol of the form where point is in.
;; If point is not inside a edebuggable form, cause error.
  (or (edebug-form-data-name (edebug-get-form-data-entry (point)))
      (error "Not inside instrumented form")))

(defun edebug-make-top-form-data-entry (new-entry)
  ;; Make NEW-ENTRY the first element in the `edebug-form-data' list.
  (edebug-clear-form-data-entry new-entry)
  (setq edebug-form-data (cons new-entry edebug-form-data)))

(defun edebug-clear-form-data-entry (entry)
;; If non-nil, clear ENTRY out of the form data.
;; Maybe clear the markers and delete the symbol's edebug property?
  (if entry
      (progn
	;; Instead of this, we could just find all contained forms.
	;; (put (car entry) 'edebug nil)   ;
	;; (mapcar 'edebug-clear-form-data-entry   ; dangerous
	;;   (get (car entry) 'edebug-dependents))
	;; (set-marker (nth 1 entry) nil)
	;; (set-marker (nth 2 entry) nil)
	(setq edebug-form-data (delq entry edebug-form-data)))))

;;; Parser utilities

(defun edebug-syntax-error (&rest args)
  ;; Signal an invalid-read-syntax with ARGS.
  (signal 'invalid-read-syntax args))


(defconst edebug-read-syntax-table
  ;; Lookup table for significant characters indicating the class of the
  ;; token that follows.  This is not a \"real\" syntax table.
  (let ((table (make-char-table 'syntax-table 'symbol))
	(i 0))
    (while (< i ?!)
      (aset table i 'space)
      (setq i (1+ i)))
    (aset table ?\( 'lparen)
    (aset table ?\) 'rparen)
    (aset table ?\' 'quote)
    (aset table ?\` 'backquote)
    (aset table ?\, 'comma)
    (aset table ?\" 'string)
    (aset table ?\? 'char)
    (aset table ?\[ 'lbracket)
    (aset table ?\] 'rbracket)
    (aset table ?\. 'dot)
    (aset table ?\# 'hash)
    ;; We treat numbers as symbols, because of confusion with -, -1, and 1-.
    ;; We don't care about any other chars since they won't be seen.
    table))

(defun edebug-next-token-class ()
  ;; Move to the next token and return its class.  We only care about
  ;; lparen, rparen, dot, quote, backquote, comma, string, char, vector,
  ;; or symbol.
  (edebug-skip-whitespace)
  (if (and (eq (following-char) ?.)
	   (save-excursion
	     (forward-char 1)
	     (or (and (eq (aref edebug-read-syntax-table (following-char))
			  'symbol)
		      (not (= (following-char) ?\;)))
		 (memq (following-char) '(?\, ?\.)))))
      'symbol
    (aref edebug-read-syntax-table (following-char))))


(defun edebug-skip-whitespace ()
  ;; Leave point before the next token, skipping white space and comments.
  (skip-chars-forward " \t\r\n\f")
  (while (= (following-char) ?\;)
    (skip-chars-forward "^\n")  ; skip the comment
    (skip-chars-forward " \t\r\n\f")))


;; Mostly obsolete reader; still used in one case.

(defun edebug-read-sexp ()
  ;; Read one sexp from the current buffer starting at point.
  ;; Leave point immediately after it.  A sexp can be a list or atom.
  ;; An atom is a symbol (or number), character, string, or vector.
  ;; This works for reading anything legitimate, but it
  ;; is gummed up by parser inconsistencies (bugs?)
  (let ((class (edebug-next-token-class)))
    (cond
     ;; read goes one too far if a (possibly quoted) string or symbol
     ;; is immediately followed by non-whitespace.
     ((eq class 'symbol) (edebug-original-read (current-buffer)))
     ((eq class 'string) (edebug-original-read (current-buffer)))
     ((eq class 'quote) (forward-char 1)
      (list 'quote (edebug-read-sexp)))
     ((eq class 'backquote)
      (list '\` (edebug-read-sexp)))
     ((eq class 'comma)
      (list '\, (edebug-read-sexp)))
     (t ; anything else, just read it.
      (edebug-original-read (current-buffer))))))

;;; Offsets for reader

;; Define a structure to represent offset positions of expressions.
;; Each offset structure looks like: (before . after) for constituents,
;; or for structures that have elements: (before <subexpressions> . after)
;; where the <subexpressions> are the offset structures for subexpressions
;; including the head of a list.
(defvar edebug-offsets nil)

;; Stack of offset structures in reverse order of the nesting.
;; This is used to get back to previous levels.
(defvar edebug-offsets-stack nil)
(defvar edebug-current-offset nil) ; Top of the stack, for convenience.

;; We must store whether we just read a list with a dotted form that
;; is itself a list.  This structure will be condensed, so the offsets
;; must also be condensed.
(defvar edebug-read-dotted-list nil)

(defsubst edebug-initialize-offsets ()
  ;; Reinitialize offset recording.
  (setq edebug-current-offset nil))

(defun edebug-store-before-offset (point)
  ;; Add a new offset pair with POINT as the before offset.
  (let ((new-offset (list point)))
    (if edebug-current-offset
	(setcdr edebug-current-offset
		(cons new-offset (cdr edebug-current-offset)))
      ;; Otherwise, we are at the top level, so initialize.
      (setq edebug-offsets new-offset
	    edebug-offsets-stack nil
	    edebug-read-dotted-list nil))
    ;; Cons the new offset to the front of the stack.
    (setq edebug-offsets-stack (cons new-offset edebug-offsets-stack)
	  edebug-current-offset new-offset)
    ))

(defun edebug-store-after-offset (point)
  ;; Finalize the current offset struct by reversing it and
  ;; store POINT as the after offset.
  (if (not edebug-read-dotted-list)
      ;; Just reverse the offsets of all subexpressions.
      (setcdr edebug-current-offset (nreverse (cdr edebug-current-offset)))

    ;; We just read a list after a dot, which will be abbreviated out.
    (setq edebug-read-dotted-list nil)
    ;; Drop the corresponding offset pair.
    ;; That is, nconc the reverse of the rest of the offsets
    ;; with the cdr of last offset.
    (setcdr edebug-current-offset
	    (nconc (nreverse (cdr (cdr edebug-current-offset)))
		   (cdr (car (cdr edebug-current-offset))))))

  ;; Now append the point using nconc.
  (setq edebug-current-offset (nconc edebug-current-offset point))
  ;; Pop the stack.
  (setq edebug-offsets-stack (cdr edebug-offsets-stack)
	edebug-current-offset (car edebug-offsets-stack)))

(defun edebug-ignore-offset ()
  ;; Ignore the last created offset pair.
  (setcdr edebug-current-offset (cdr (cdr edebug-current-offset))))

(defmacro edebug-storing-offsets (point &rest body)
  (declare (debug (form body)) (indent 1))
  `(unwind-protect
       (progn
	 (edebug-store-before-offset ,point)
	 ,@body)
     (edebug-store-after-offset (point))))


;;; Reader for Emacs Lisp.

;; Uses edebug-next-token-class (and edebug-skip-whitespace) above.

(defconst edebug-read-alist
  '((symbol . edebug-read-symbol)
    (lparen . edebug-read-list)
    (string . edebug-read-string)
    (quote . edebug-read-quote)
    (backquote . edebug-read-backquote)
    (comma . edebug-read-comma)
    (lbracket . edebug-read-vector)
    (hash . edebug-read-function)
    ))

(defun edebug-read-storing-offsets (stream)
  (let (edebug-read-dotted-list) ; see edebug-store-after-offset
    (edebug-storing-offsets (point)
      (funcall
       (or (cdr (assq (edebug-next-token-class) edebug-read-alist))
	   ;; anything else, just read it.
	   'edebug-original-read)
       stream))))

(defun edebug-read-symbol (stream)
  (edebug-original-read stream))

(defun edebug-read-string (stream)
  (edebug-original-read stream))

(defun edebug-read-quote (stream)
  ;; Turn 'thing into (quote thing)
  (forward-char 1)
  (list
   (edebug-storing-offsets (1- (point)) 'quote)
   (edebug-read-storing-offsets stream)))

(defun edebug-read-backquote (stream)
  ;; Turn `thing into (\` thing)
  (forward-char 1)
  (list
   (edebug-storing-offsets (1- (point)) '\`)
   (edebug-read-storing-offsets stream)))

(defun edebug-read-comma (stream)
  ;; Turn ,thing into (\, thing).  Handle ,@ and ,. also.
  (let ((opoint (point)))
    (forward-char 1)
    (let ((symbol '\,))
      (cond ((eq (following-char) ?\.)
	     (setq symbol '\,\.)
	     (forward-char 1))
	    ((eq (following-char) ?\@)
	     (setq symbol '\,@)
	     (forward-char 1)))
      ;; Generate the same structure of offsets we would have
      ;; if the resulting list appeared verbatim in the input text.
      (list
       (edebug-storing-offsets opoint symbol)
       (edebug-read-storing-offsets stream)))))

(defun edebug-read-function (stream)
  ;; Turn #'thing into (function thing)
  (forward-char 1)
  (cond ((eq ?\' (following-char))
	 (forward-char 1)
	 (list
	  (edebug-storing-offsets (- (point) 2)
	    (if (featurep 'cl) 'function* 'function))
	  (edebug-read-storing-offsets stream)))
	((memq (following-char) '(?: ?B ?O ?X ?b ?o ?x ?1 ?2 ?3 ?4 ?5 ?6
				  ?7 ?8 ?9 ?0))
	 (backward-char 1)
	 (edebug-original-read stream))
	(t (edebug-syntax-error "Bad char after #"))))

(defun edebug-read-list (stream)
  (forward-char 1)			; skip \(
  (prog1
      (let ((elements))
	(while (not (memq (edebug-next-token-class) '(rparen dot)))
          (push (edebug-read-storing-offsets stream) elements))
	(setq elements (nreverse elements))
	(if (eq 'dot (edebug-next-token-class))
	    (let (dotted-form)
	      (forward-char 1)		; skip \.
	      (setq dotted-form (edebug-read-storing-offsets stream))
		    elements (nconc elements dotted-form)
	      (if (not (eq (edebug-next-token-class) 'rparen))
		  (edebug-syntax-error "Expected `)'"))
	      (setq edebug-read-dotted-list (listp dotted-form))
	      ))
	elements)
    (forward-char 1)			; skip \)
    ))

(defun edebug-read-vector (stream)
  (forward-char 1)			; skip \[
  (prog1
      (let ((elements))
	(while (not (eq 'rbracket (edebug-next-token-class)))
	  (push (edebug-read-storing-offsets stream) elements))
	(apply 'vector (nreverse elements)))
    (forward-char 1)			; skip \]
    ))

;;; Cursors for traversal of list and vector elements with offsets.

(defvar edebug-dotted-spec nil)

(defun edebug-new-cursor (expressions offsets)
  ;; Return a new cursor for EXPRESSIONS with OFFSETS.
  (if (vectorp expressions)
      (setq expressions (append expressions nil)))
  (cons expressions offsets))

(defsubst edebug-set-cursor (cursor expressions offsets)
  ;; Set the CURSOR's EXPRESSIONS and OFFSETS to the given.
  ;; Return the cursor.
  (setcar cursor expressions)
  (setcdr cursor offsets)
  cursor)

(defun edebug-copy-cursor (cursor)
  ;; Copy the cursor using the same object and offsets.
  (cons (car cursor) (cdr cursor)))

(defsubst edebug-cursor-expressions (cursor)
  (car cursor))
(defsubst edebug-cursor-offsets (cursor)
  (cdr cursor))

(defsubst edebug-empty-cursor (cursor)
  ;; Return non-nil if CURSOR is empty - meaning no more elements.
  (null (car cursor)))

(defsubst edebug-top-element (cursor)
  ;; Return the top element at the cursor.
  ;; Assumes not empty.
  (car (car cursor)))

(defun edebug-top-element-required (cursor &rest error)
  ;; Check if a dotted form is required.
  (if edebug-dotted-spec (edebug-no-match cursor "Dot expected."))
  ;; Check if there is at least one more argument.
  (if (edebug-empty-cursor cursor) (apply 'edebug-no-match cursor error))
  ;; Return that top element.
  (edebug-top-element cursor))

(defsubst edebug-top-offset (cursor)
  ;; Return the top offset pair corresponding to the top element.
  (car (cdr cursor)))

(defun edebug-move-cursor (cursor)
  ;; Advance and return the cursor to the next element and offset.
  ;; throw no-match if empty before moving.
  ;; This is a violation of the cursor encapsulation, but
  ;; there is plenty of that going on while matching.
  ;; The following test should always fail.
  (if (edebug-empty-cursor cursor)
      (edebug-no-match cursor "Not enough arguments."))
  (setcar cursor (cdr (car cursor)))
  (setcdr cursor (cdr (cdr cursor)))
  cursor)


(defun edebug-before-offset (cursor)
  ;; Return the before offset of the cursor.
  ;; If there is nothing left in the offsets,
  ;; return one less than the offset itself,
  ;; which is the after offset for a list.
  (let ((offset (edebug-cursor-offsets cursor)))
    (if (consp offset)
	(car (car offset))
      (1- offset))))

(defun edebug-after-offset (cursor)
  ;; Return the after offset of the cursor object.
  (let ((offset (edebug-top-offset cursor)))
    (while (consp offset)
      (setq offset (cdr offset)))
    offset))

;;; The Parser

;; The top level function for parsing forms is
;; edebug-read-and-maybe-wrap-form; it calls all the rest.  It checks the
;; syntax a bit and leaves point at any error it finds, but otherwise
;; should appear to work like eval-defun.

;; The basic plan is to surround each expression with a call to
;; the edebug debugger together with indexes into a table of positions of
;; all expressions.  Thus an expression "exp" becomes:

;; (edebug-after (edebug-before 1) 2 exp)

;; When this is evaluated, first point is moved to the beginning of
;; exp at offset 1 of the current function.  The expression is
;; evaluated, which may cause more edebug calls, and then point is
;; moved to offset 2 after the end of exp.

;; The highest level expressions of the function are wrapped in a call to
;; edebug-enter, which supplies the function name and the actual
;; arguments to the function.  See functions edebug-enter, edebug-before,
;; and edebug-after for more details.

;; Dynamically bound vars, left unbound, but globally declared.
;; This is to quiet the byte compiler.

;; Window data of the highest definition being wrapped.
;; This data is shared by all embedded definitions.
(defvar edebug-top-window-data)

(defvar edebug-&optional)
(defvar edebug-&rest)
(defvar edebug-gate nil) ;; whether no-match forces an error.

(defvar edebug-def-name nil) ; name of definition, used by interactive-form
(defvar edebug-old-def-name nil) ; previous name of containing definition.

(defvar edebug-error-point nil)
(defvar edebug-best-error nil)


(defun edebug-read-and-maybe-wrap-form ()
  ;; Read a form and wrap it with edebug calls, if the conditions are right.
  ;; Here we just catch any no-match not caught below and signal an error.

  ;; Run the setup hook.
  ;; If it gets an error, make it nil.
  (let ((temp-hook edebug-setup-hook))
    (setq edebug-setup-hook nil)
    (run-hooks 'temp-hook))

  (let (result
	edebug-top-window-data
	edebug-def-name;; make sure it is locally nil
	;; I don't like these here!!
	edebug-&optional
	edebug-&rest
	edebug-gate
	edebug-best-error
	edebug-error-point
	no-match
	;; Do this once here instead of several times.
	(max-lisp-eval-depth (+ 800 max-lisp-eval-depth))
	(max-specpdl-size (+ 2000 max-specpdl-size)))
    (setq no-match
	  (catch 'no-match
	    (setq result (edebug-read-and-maybe-wrap-form1))
	    nil))
    (if no-match
	(apply 'edebug-syntax-error no-match))
    result))


(defun edebug-read-and-maybe-wrap-form1 ()
  (let (spec
	def-kind
	defining-form-p
	def-name
	;; These offset things don't belong here, but to support recursive
	;; calls to edebug-read, they need to be here.
	edebug-offsets
	edebug-offsets-stack
	edebug-current-offset ; reset to nil
	)
    (save-excursion
      (if (and (eq 'lparen (edebug-next-token-class))
	       (eq 'symbol (progn (forward-char 1) (edebug-next-token-class))))
	  ;; Find out if this is a defining form from first symbol
	  (setq def-kind (edebug-original-read (current-buffer))
		spec (and (symbolp def-kind) (get-edebug-spec def-kind))
		defining-form-p (and (listp spec)
				     (eq '&define (car spec)))
		;; This is incorrect in general!! But OK most of the time.
		def-name (if (and defining-form-p
				  (eq 'name (car (cdr spec)))
				  (eq 'symbol (edebug-next-token-class)))
			     (edebug-original-read (current-buffer))))))
;;;(message "all defs: %s   all forms: %s"  edebug-all-defs edebug-all-forms)
    (cond
     (defining-form-p
       (if (or edebug-all-defs edebug-all-forms)
	   ;; If it is a defining form and we are edebugging defs,
	   ;; then let edebug-list-form start it.
	   (let ((cursor (edebug-new-cursor
			  (list (edebug-read-storing-offsets (current-buffer)))
			  (list edebug-offsets))))
	     (car
	      (edebug-make-form-wrapper
	       cursor
	       (edebug-before-offset cursor)
	       (1- (edebug-after-offset cursor))
	       (list (cons (symbol-name def-kind) (cdr spec))))))

	 ;; Not edebugging this form, so reset the symbol's edebug
	 ;; property to be just a marker at the definition's source code.
	 ;; This only works for defs with simple names.
	 (put def-name 'edebug (point-marker))
	 ;; Also nil out dependent defs.
	 '(mapcar (function
		   (lambda (def)
		     (put def-name 'edebug nil)))
		  (get def-name 'edebug-dependents))
	 (edebug-read-sexp)))

     ;; If all forms are being edebugged, explicitly wrap it.
     (edebug-all-forms
      (let ((cursor (edebug-new-cursor
		     (list (edebug-read-storing-offsets (current-buffer)))
		     (list edebug-offsets))))
	(edebug-make-form-wrapper
	 cursor
	 (edebug-before-offset cursor)
	 (edebug-after-offset cursor)
	 nil)))

     ;; Not a defining form, and not edebugging.
     (t (edebug-read-sexp)))
    ))


(defvar edebug-def-args) ; args of defining form.
(defvar edebug-def-interactive) ; is it an emacs interactive function?
(defvar edebug-inside-func)  ;; whether code is inside function context.
;; Currently def-form sets this to nil; def-body sets it to t.

(defun edebug-interactive-p-name ()
  ;; Return a unique symbol for the variable used to store the
  ;; status of interactive-p for this function.
  (intern (format "edebug-%s-interactive-p" edebug-def-name)))


(defun edebug-wrap-def-body (forms)
  "Wrap the FORMS of a definition body."
  (if edebug-def-interactive
      `(let ((,(edebug-interactive-p-name)
	      (interactive-p)))
	 ,(edebug-make-enter-wrapper forms))
    (edebug-make-enter-wrapper forms)))


(defun edebug-make-enter-wrapper (forms)
  ;; Generate the enter wrapper for some forms of a definition.
  ;; This is not to be used for the body of other forms, e.g. `while',
  ;; since it wraps the list of forms with a call to `edebug-enter'.
  ;; Uses the dynamically bound vars edebug-def-name and edebug-def-args.
  ;; Do this after parsing since that may find a name.
  (setq edebug-def-name
	(or edebug-def-name edebug-old-def-name (edebug-gensym "edebug-anon")))
  `(edebug-enter
    (quote ,edebug-def-name)
    ,(if edebug-inside-func
	 `(list
	   ;; Doesn't work with more than one def-body!!
	   ;; But the list will just be reversed.
	   ,@(nreverse edebug-def-args))
       'nil)
    (function (lambda () ,@forms))
    ))


(defvar edebug-form-begin-marker) ; the mark for def being instrumented

(defvar edebug-offset-index) ; the next available offset index.
(defvar edebug-offset-list) ; the list of offset positions.

(defun edebug-inc-offset (offset)
  ;; modifies edebug-offset-index and edebug-offset-list
  ;; accesses edebug-func-marc and buffer point
  (prog1
      edebug-offset-index
    (setq edebug-offset-list (cons (- offset edebug-form-begin-marker)
				   edebug-offset-list)
	  edebug-offset-index (1+ edebug-offset-index))))


(defun edebug-make-before-and-after-form (before-index form after-index)
  ;; Return the edebug form for the current function at offset BEFORE-INDEX
  ;; given FORM.  Looks like:
  ;; (edebug-after (edebug-before BEFORE-INDEX) AFTER-INDEX FORM)
  ;; Also increment the offset index for subsequent use.
  (list 'edebug-after
	(list 'edebug-before before-index)
	after-index form))

(defun edebug-make-after-form (form after-index)
  ;; Like edebug-make-before-and-after-form, but only after.
  (list 'edebug-after 0 after-index form))


(defun edebug-unwrap (sexp)
  "Return the unwrapped SEXP or return it as is if it is not wrapped.
The SEXP might be the result of wrapping a body, which is a list of
expressions; a `progn' form will be returned enclosing these forms."
  (if (consp sexp)
      (cond
       ((eq 'edebug-after (car sexp))
	(nth 3 sexp))
       ((eq 'edebug-enter (car sexp))
	(let ((forms (nthcdr 2 (nth 1 (nth 3 sexp)))))
	  (if (> (length forms) 1)
	      (cons 'progn forms)  ;; could return (values forms) instead.
	    (car forms))))
       (t sexp);; otherwise it is not wrapped, so just return it.
       )
    sexp))

(defun edebug-unwrap* (sexp)
  "Return the SEXP recursively unwrapped."
  (let ((new-sexp (edebug-unwrap sexp)))
    (while (not (eq sexp new-sexp))
      (setq sexp new-sexp
	    new-sexp (edebug-unwrap sexp)))
    (if (consp new-sexp)
	(mapcar 'edebug-unwrap* new-sexp)
      new-sexp)))


(defun edebug-defining-form (cursor form-begin form-end speclist)
  ;; Process the defining form, starting outside the form.
  ;; The speclist is a generated list spec that looks like:
  ;;   (("def-symbol" defining-form-spec-sans-&define))
  ;; Skip the first offset.
  (edebug-set-cursor cursor (edebug-cursor-expressions cursor)
		     (cdr (edebug-cursor-offsets cursor)))
  (edebug-make-form-wrapper
   cursor
   form-begin (1- form-end)
   speclist))

(defun edebug-make-form-wrapper (cursor form-begin form-end
					&optional speclist)
  ;; Wrap a form, usually a defining form, but any evaluated one.
  ;; If speclist is non-nil, this is being called by edebug-defining-form.
  ;; Otherwise it is being called from edebug-read-and-maybe-wrap-form1.
  ;; This is a hack, but I haven't figured out a simpler way yet.
  (let* ((form-data-entry (edebug-get-form-data-entry form-begin form-end))
	 ;; Set this marker before parsing.
	 (edebug-form-begin-marker
	  (if form-data-entry
	      (edebug-form-data-begin form-data-entry)
	    ;; Buffer must be current-buffer for this to work:
	    (set-marker (make-marker) form-begin))))

    (let (edebug-offset-list
	  (edebug-offset-index 0)
	  result
	  ;; For definitions.
	  ;; (edebug-containing-def-name edebug-def-name)
	  ;; Get name from form-data, if any.
	  (edebug-old-def-name (edebug-form-data-name form-data-entry))
	  edebug-def-name
	  edebug-def-args
	  edebug-def-interactive
	  edebug-inside-func;; whether wrapped code executes inside a function.
	  )

      (setq result
	    (if speclist
		(edebug-match cursor speclist)

	      ;; else wrap as an enter-form.
	      (edebug-make-enter-wrapper (list (edebug-form cursor)))))

      ;; Set the name here if it was not set by edebug-make-enter-wrapper.
      (setq edebug-def-name
	    (or edebug-def-name edebug-old-def-name (edebug-gensym "edebug-anon")))

      ;; Add this def as a dependent of containing def.  Buggy.
      '(if (and edebug-containing-def-name
		(not (get edebug-containing-def-name 'edebug-dependents)))
	   (put edebug-containing-def-name 'edebug-dependents
		(cons edebug-def-name
		      (get edebug-containing-def-name
			   'edebug-dependents))))

      ;; Create a form-data-entry or modify existing entry's markers.
      ;; In the latter case, pointers to the entry remain eq.
      (if (not form-data-entry)
	  (setq form-data-entry
		(edebug-make-form-data-entry
		 edebug-def-name
		 edebug-form-begin-marker
		 ;; Buffer must be current-buffer.
		 (set-marker (make-marker) form-end)
		 ))
	(edebug-set-form-data-entry
	 form-data-entry edebug-def-name ;; in case name is changed
	 form-begin form-end))

      ;;    (message "defining: %s" edebug-def-name) (sit-for 2)
      (edebug-make-top-form-data-entry form-data-entry)
      (message "Edebug: %s" edebug-def-name)
      ;;(debug edebug-def-name)

      ;; Destructively reverse edebug-offset-list and make vector from it.
      (setq edebug-offset-list (vconcat (nreverse edebug-offset-list)))

      ;; Side effects on the property list of edebug-def-name.
      (edebug-clear-frequency-count edebug-def-name)
      (edebug-clear-coverage edebug-def-name)

      ;; Set up the initial window data.
      (if (not edebug-top-window-data) ;; if not already set, do it now.
	  (let ((window ;; Find the best window for this buffer.
		 (or (get-buffer-window (current-buffer))
		     (selected-window))))
	    (setq edebug-top-window-data
		  (cons window (window-start window)))))

      ;; Store the edebug data in symbol's property list.
      (put edebug-def-name 'edebug
	   ;; A struct or vector would be better here!!
	   (list edebug-form-begin-marker
		 nil			; clear breakpoints
		 edebug-offset-list
		 edebug-top-window-data
		 ))
      result
      )))


(defun edebug-clear-frequency-count (name)
  ;; Create initial frequency count vector.
  ;; For each stop point, the counter is incremented each time it is visited.
  (put name 'edebug-freq-count
       (make-vector (length edebug-offset-list) 0)))


(defun edebug-clear-coverage (name)
  ;; Create initial coverage vector.
  ;; Only need one per expression, but it is simpler to use stop points.
  (put name 'edebug-coverage
       (make-vector (length edebug-offset-list) 'unknown)))


(defun edebug-form (cursor)
  ;; Return the instrumented form for the following form.
  ;; Add the point offsets to the edebug-offset-list for the form.
  (let* ((form (edebug-top-element-required cursor "Expected form"))
	 (offset (edebug-top-offset cursor)))
    (prog1
	(cond
	 ((consp form)
	  ;; The first offset for a list form is for the list form itself.
	  (if (eq 'quote (car form))
	      form
	    (let* ((head (car form))
		   (spec (and (symbolp head) (get-edebug-spec head)))
		   (new-cursor (edebug-new-cursor form offset)))
	      ;; Find out if this is a defining form from first symbol.
	      ;; An indirect spec would not work here, yet.
	      (if (and (consp spec) (eq '&define (car spec)))
		  (edebug-defining-form
		   new-cursor
		   (car offset);; before the form
		   (edebug-after-offset cursor)
		   (cons (symbol-name head) (cdr spec)))
		;; Wrap a regular form.
		(edebug-make-before-and-after-form
		 (edebug-inc-offset (car offset))
		 (edebug-list-form new-cursor)
		 ;; After processing the list form, the new-cursor is left
		 ;; with the offset after the form.
		 (edebug-inc-offset (edebug-cursor-offsets new-cursor))))
	      )))

	 ((symbolp form)
	  (cond
	   ;; Check for constant symbols that don't get wrapped.
	   ((or (memq form '(t nil))
		(keywordp form))
	    form)

	   (t ;; just a variable
	    (edebug-make-after-form form (edebug-inc-offset (cdr offset))))))

	 ;; Anything else is self-evaluating.
	 (t form))
    (edebug-move-cursor cursor))))


(defsubst edebug-forms (cursor)  (edebug-match cursor '(&rest form)))
(defsubst edebug-sexps (cursor)  (edebug-match cursor '(&rest sexp)))

(defsubst edebug-list-form-args (head cursor)
  ;; Process the arguments of a list form given that head of form is a symbol.
  ;; Helper for edebug-list-form
  (let ((spec (get-edebug-spec head)))
    (cond
     (spec
      (cond
       ((consp spec)
	;; It is a speclist.
	(let (edebug-best-error
	      edebug-error-point);; This may not be needed.
	  (edebug-match-sublist cursor spec)))
       ((eq t spec) (edebug-forms cursor))
       ((eq 0 spec) (edebug-sexps cursor))
       ((symbolp spec) (funcall spec cursor));; Not used by edebug,
					; but leave it in for compatibility.
       ))
     ;; No edebug-form-spec provided.
     ((edebug-macrop head)
      (if edebug-eval-macro-args
	  (edebug-forms cursor)
	(edebug-sexps cursor)))
     (t ;; Otherwise it is a function call.
      (edebug-forms cursor)))))


(defun edebug-list-form (cursor)
  ;; Return an instrumented form built from the list form.
  ;; The after offset will be left in the cursor after processing the form.
  (let ((head (edebug-top-element-required cursor "Expected elements"))
	;; Prevent backtracking whenever instrumenting.
	(edebug-gate t)
	;; A list form is never optional because it matches anything.
	(edebug-&optional nil)
	(edebug-&rest nil))
    ;; Skip the first offset.
    (edebug-set-cursor cursor (edebug-cursor-expressions cursor)
		       (cdr (edebug-cursor-offsets cursor)))
    (cond
     ((symbolp head)
      (cond
       ((null head) nil) ; () is valid.
       ((eq head 'interactive-p)
	;; Special case: replace (interactive-p) with variable
	(setq edebug-def-interactive 'check-it)
	(edebug-move-cursor cursor)
	(edebug-interactive-p-name))
       (t
	(cons head (edebug-list-form-args
		    head (edebug-move-cursor cursor))))))

     ((consp head)
      (if (eq (car head) '\,)
	  ;; The head of a form should normally be a symbol or a lambda
	  ;; expression but it can also be an unquote form to be filled
	  ;; before evaluation.  We evaluate the arguments anyway, on the
	  ;; assumption that the unquote form will place a proper function
	  ;; name (rather than a macro name).
	  (edebug-match cursor '(("," def-form) body))
	;; Process anonymous function and args.
	;; This assumes no anonymous macros.
	(edebug-match-specs cursor '(lambda-expr body) 'edebug-match-specs)))

     (t (edebug-syntax-error
	 "Head of list form must be a symbol or lambda expression")))
      ))

;;; Matching of specs.

(defvar edebug-after-dotted-spec nil)

(defvar edebug-matching-depth 0)  ;; initial value
(defconst edebug-max-depth 150)  ;; maximum number of matching recursions.


;;; Failure to match

;; This throws to no-match, if there are higher alternatives.
;; Otherwise it signals an error.  The place of the error is found
;; with the two before- and after-offset functions.

(defun edebug-no-match (cursor &rest edebug-args)
  ;; Throw a no-match, or signal an error immediately if gate is active.
  ;; Remember this point in case we need to report this error.
  (setq edebug-error-point (or edebug-error-point
			       (edebug-before-offset cursor))
	edebug-best-error (or edebug-best-error edebug-args))
  (if (and edebug-gate (not edebug-&optional))
      (progn
	(if edebug-error-point
	    (goto-char edebug-error-point))
	(apply 'edebug-syntax-error edebug-args))
    (funcall 'throw 'no-match edebug-args)))


(defun edebug-match (cursor specs)
  ;; Top level spec matching function.
  ;; Used also at each lower level of specs.
  (let (edebug-&optional
	edebug-&rest
	edebug-best-error
	edebug-error-point
	(edebug-gate edebug-gate)  ;; locally bound to limit effect
	)
    (edebug-match-specs cursor specs 'edebug-match-specs)))


(defun edebug-match-one-spec (cursor spec)
  ;; Match one spec, which is not a keyword &-spec.
  (cond
   ((symbolp spec) (edebug-match-symbol cursor spec))
   ((vectorp spec) (edebug-match cursor (append spec nil)))
   ((stringp spec) (edebug-match-string cursor spec))
   ((listp spec) (edebug-match-list cursor spec))
   ))


(defun edebug-match-specs (cursor specs remainder-handler)
  ;; Append results of matching the list of specs.
  ;; The first spec is handled and the remainder-handler handles the rest.
  (let ((edebug-matching-depth
	 (if (> edebug-matching-depth edebug-max-depth)
	     (error "Too deep - perhaps infinite loop in spec?")
	   (1+ edebug-matching-depth))))
    (cond
     ((null specs) nil)

     ;; Is the spec dotted?
     ((atom specs)
      (let ((edebug-dotted-spec t));; Containing spec list was dotted.
	(edebug-match-specs cursor (list specs) remainder-handler)))

     ;; Is the form dotted?
     ((not (listp (edebug-cursor-expressions cursor)));; allow nil
      (if (not edebug-dotted-spec)
	  (edebug-no-match cursor "Dotted spec required."))
      ;; Cancel dotted spec and dotted form.
      (let ((edebug-dotted-spec)
	    (this-form (edebug-cursor-expressions cursor))
	    (this-offset (edebug-cursor-offsets cursor)))
	;; Wrap the form in a list, (by changing the cursor??)...
	(edebug-set-cursor cursor (list this-form) this-offset)
	;; and process normally, then unwrap the result.
	(car (edebug-match-specs cursor specs remainder-handler))))

     (t;; Process normally.
      (let* ((spec (car specs))
	     (rest)
	     (first-char (and (symbolp spec) (aref (symbol-name spec) 0))))
	;;(message "spec = %s  first char = %s" spec first-char) (sit-for 1)
	(nconc
	 (cond
	  ((eq ?& first-char);; "&" symbols take all following specs.
	   (funcall (get-edebug-spec spec) cursor (cdr specs)))
	  ((eq ?: first-char);; ":" symbols take one following spec.
	   (setq rest (cdr (cdr specs)))
	   (funcall (get-edebug-spec spec) cursor (car (cdr specs))))
	  (t;; Any other normal spec.
	   (setq rest (cdr specs))
	   (edebug-match-one-spec cursor spec)))
	 (funcall remainder-handler cursor rest remainder-handler)))))))


;; Define specs for all the symbol specs with functions used to process them.
;; Perhaps we shouldn't be doing this with edebug-form-specs since the
;; user may want to define macros or functions with the same names.
;; We could use an internal obarray for these primitive specs.

(dolist (pair '((&optional . edebug-match-&optional)
		(&rest . edebug-match-&rest)
		(&or . edebug-match-&or)
		(form . edebug-match-form)
		(sexp . edebug-match-sexp)
		(body . edebug-match-body)
		(&define . edebug-match-&define)
		(name . edebug-match-name)
		(:name . edebug-match-colon-name)
		(arg . edebug-match-arg)
		(def-body . edebug-match-def-body)
		(def-form . edebug-match-def-form)
		;; Less frequently used:
		;; (function . edebug-match-function)
		(lambda-expr . edebug-match-lambda-expr)
		(&not . edebug-match-&not)
		(&key . edebug-match-&key)
		(place . edebug-match-place)
		(gate . edebug-match-gate)
		;;   (nil . edebug-match-nil)  not this one - special case it.
		))
  (put (car pair) 'edebug-form-spec (cdr pair)))

(defun edebug-match-symbol (cursor symbol)
  ;; Match a symbol spec.
  (let* ((spec (get-edebug-spec symbol)))
    (cond
     (spec
      (if (consp spec)
	  ;; It is an indirect spec.
	  (edebug-match cursor spec)
	;; Otherwise it should be the symbol name of a function.
	;; There could be a bug here - maybe need to do edebug-match bindings.
	(funcall spec cursor)))

     ((null symbol)  ;; special case this.
      (edebug-match-nil cursor))

     ((fboundp symbol)			; is it a predicate?
      (let ((sexp (edebug-top-element-required cursor "Expected" symbol)))
	;; Special case for edebug-`.
	(if (and (listp sexp) (eq (car sexp) '\,))
	    (edebug-match cursor '(("," def-form)))
	  (if (not (funcall symbol sexp))
	      (edebug-no-match cursor symbol "failed"))
	  (edebug-move-cursor cursor)
	  (list sexp))))
     (t (error "%s is not a form-spec or function" symbol))
     )))


(defun edebug-match-sexp (cursor)
  (list (prog1 (edebug-top-element-required cursor "Expected sexp")
	  (edebug-move-cursor cursor))))

(defun edebug-match-form (cursor)
  (list (edebug-form cursor)))

(defalias 'edebug-match-place 'edebug-match-form)
  ;; Currently identical to edebug-match-form.
  ;; This is for common lisp setf-style place arguments.

(defsubst edebug-match-body (cursor) (edebug-forms cursor))

(defun edebug-match-&optional (cursor specs)
  ;; Keep matching until one spec fails.
  (edebug-&optional-wrapper cursor specs 'edebug-&optional-wrapper))

(defun edebug-&optional-wrapper (cursor specs remainder-handler)
  (let (result
	(edebug-&optional specs)
	(edebug-gate nil)
	(this-form (edebug-cursor-expressions cursor))
	(this-offset (edebug-cursor-offsets cursor)))
    (if (null (catch 'no-match
		(setq result
		      (edebug-match-specs cursor specs remainder-handler))
		;; Returning nil means no no-match was thrown.
		nil))
	result
      ;; no-match, but don't fail; just reset cursor and return nil.
      (edebug-set-cursor cursor this-form this-offset)
      nil)))


(defun edebug-&rest-wrapper (cursor specs remainder-handler)
  (if (null specs) (setq specs edebug-&rest))
  ;; Reuse the &optional handler with this as the remainder handler.
  (edebug-&optional-wrapper cursor specs remainder-handler))

(defun edebug-match-&rest (cursor specs)
  ;; Repeatedly use specs until failure.
  (let ((edebug-&rest specs) ;; remember these
	edebug-best-error
	edebug-error-point)
    (edebug-&rest-wrapper cursor specs 'edebug-&rest-wrapper)))


(defun edebug-match-&or (cursor specs)
  ;; Keep matching until one spec succeeds, and return its results.
  ;; If none match, fail.
  ;; This needs to be optimized since most specs spend time here.
  (let ((original-specs specs)
	(this-form (edebug-cursor-expressions cursor))
	(this-offset (edebug-cursor-offsets cursor)))
    (catch 'matched
      (while specs
	(catch 'no-match
	  (throw 'matched
		 (let (edebug-gate ;; only while matching each spec
		       edebug-best-error
		       edebug-error-point)
		   ;; Doesn't support e.g. &or symbolp &rest form
		   (edebug-match-one-spec cursor (car specs)))))
	;; Match failed, so reset and try again.
	(setq specs (cdr specs))
	;; Reset the cursor for the next match.
	(edebug-set-cursor cursor this-form this-offset))
      ;; All failed.
      (apply 'edebug-no-match cursor "Expected one of" original-specs))
    ))


(defun edebug-match-&not (cursor specs)
  ;; If any specs match, then fail
  (if (null (catch 'no-match
	      (let ((edebug-gate nil))
		(save-excursion
		  (edebug-match-&or cursor specs)))
	      nil))
      ;; This means something matched, so it is a no match.
      (edebug-no-match cursor "Unexpected"))
  ;; This means nothing matched, so it is OK.
  nil) ;; So, return nothing


(def-edebug-spec &key edebug-match-&key)

(defun edebug-match-&key (cursor specs)
  ;; Following specs must look like (<name> <spec>) ...
  ;; where <name> is the name of a keyword, and spec is its spec.
  ;; This really doesn't save much over the expanded form and takes time.
  (edebug-match-&rest
   cursor
   (cons '&or
	 (mapcar (function (lambda (pair)
			     (vector (format ":%s" (car pair))
				     (car (cdr pair)))))
		 specs))))


(defun edebug-match-gate (cursor)
  ;; Simply set the gate to prevent backtracking at this level.
  (setq edebug-gate t)
  nil)


(defun edebug-match-list (cursor specs)
  ;; The spec is a list, but what kind of list, and what context?
  (if edebug-dotted-spec
      ;; After dotted spec but form did not contain dot,
      ;; so match list spec elements as if spliced in.
      (prog1
	  (let ((edebug-dotted-spec))
	    (edebug-match-specs cursor specs 'edebug-match-specs))
	;; If it matched, really clear the dotted-spec flag.
	(setq edebug-dotted-spec nil))
    (let ((spec (car specs))
	  (form (edebug-top-element-required cursor "Expected" specs)))
      (cond
       ((eq 'quote spec)
	(let ((spec (car (cdr specs))))
	  (cond
	   ((symbolp spec)
	    ;; Special case: spec quotes a symbol to match.
	    ;; Change in future.  Use "..." instead.
	    (if (not (eq spec form))
		(edebug-no-match cursor "Expected" spec))
	    (edebug-move-cursor cursor)
	    (setq edebug-gate t)
	    form)
	   (t
	    (error "Bad spec: %s" specs)))))

       ((listp form)
	(prog1
	    (list (edebug-match-sublist
		   ;; First offset is for the list form itself.
		   ;; Treat nil as empty list.
		   (edebug-new-cursor form (cdr (edebug-top-offset cursor)))
		   specs))
	  (edebug-move-cursor cursor)))

       ((and (eq 'vector spec) (vectorp form))
	;; Special case: match a vector with the specs.
	(let ((result (edebug-match-sublist
		       (edebug-new-cursor
			form (cdr (edebug-top-offset cursor)))
		       (cdr specs))))
	  (edebug-move-cursor cursor)
	  (list (apply 'vector result))))

       (t (edebug-no-match cursor "Expected" specs)))
      )))


(defun edebug-match-sublist (cursor specs)
  ;; Match a sublist of specs.
  (let (edebug-&optional
	;;edebug-best-error
	;;edebug-error-point
	)
    (prog1
	;; match with edebug-match-specs so edebug-best-error is not bound.
	(edebug-match-specs cursor specs 'edebug-match-specs)
      (if (not (edebug-empty-cursor cursor))
	  (if edebug-best-error
	      (apply 'edebug-no-match cursor edebug-best-error)
	    ;; A failed &rest or &optional spec may leave some args.
	    (edebug-no-match cursor "Failed matching" specs)
	    )))))


(defun edebug-match-string (cursor spec)
  (let ((sexp (edebug-top-element-required cursor "Expected" spec)))
    (if (not (eq (intern spec) sexp))
	(edebug-no-match cursor "Expected" spec)
      ;; Since it matched, failure means immediate error, unless &optional.
      (setq edebug-gate t)
      (edebug-move-cursor cursor)
      (list sexp)
      )))

(defun edebug-match-nil (cursor)
  ;; There must be nothing left to match a nil.
  (if (not (edebug-empty-cursor cursor))
      (edebug-no-match cursor "Unmatched argument(s)")
    nil))


(defun edebug-match-function (cursor)
  (error "Use function-form instead of function in edebug spec"))

(defun edebug-match-&define (cursor specs)
  ;; Match a defining form.
  ;; Normally, &define is interpreted specially other places.
  ;; This should only be called inside of a spec list to match the remainder
  ;; of the current list.  e.g. ("lambda" &define args def-body)
   (edebug-make-form-wrapper
    cursor
    (edebug-before-offset cursor)
    ;; Find the last offset in the list.
    (let ((offsets (edebug-cursor-offsets cursor)))
      (while (consp offsets) (setq offsets (cdr offsets)))
      offsets)
    specs))

(defun edebug-match-lambda-expr (cursor)
  ;; The expression must be a function.
  ;; This will match any list form that begins with a symbol
  ;; that has an edebug-form-spec beginning with &define.  In
  ;; practice, only lambda expressions should be used.
  ;; I could add a &lambda specification to avoid confusion.
  (let* ((sexp (edebug-top-element-required
		cursor "Expected lambda expression"))
	 (offset (edebug-top-offset cursor))
	 (head (and (consp sexp) (car sexp)))
	 (spec (and (symbolp head) (get-edebug-spec head)))
	 (edebug-inside-func nil))
    ;; Find out if this is a defining form from first symbol.
    (if (and (consp spec) (eq '&define (car spec)))
	(prog1
	    (list
	     (edebug-defining-form
	      (edebug-new-cursor sexp offset)
	      (car offset);; before the sexp
	      (edebug-after-offset cursor)
	      (cons (symbol-name head) (cdr spec))))
	  (edebug-move-cursor cursor))
      (edebug-no-match cursor "Expected lambda expression")
      )))


(defun edebug-match-name (cursor)
  ;; Set the edebug-def-name bound in edebug-defining-form.
  (let ((name (edebug-top-element-required cursor "Expected name")))
    ;; Maybe strings and numbers could be used.
    (if (not (symbolp name))
	(edebug-no-match cursor "Symbol expected for name of definition"))
    (setq edebug-def-name
	  (if edebug-def-name
	      ;; Construct a new name by appending to previous name.
	      (intern (format "%s@%s" edebug-def-name name))
	    name))
    (edebug-move-cursor cursor)
    (list name)))

(defun edebug-match-colon-name (cursor spec)
  ;; Set the edebug-def-name to the spec.
  (setq edebug-def-name
	(if edebug-def-name
	    ;; Construct a new name by appending to previous name.
	    (intern (format "%s@%s" edebug-def-name spec))
	  spec))
  nil)

(defun edebug-match-arg (cursor)
  ;; set the def-args bound in edebug-defining-form
  (let ((edebug-arg (edebug-top-element-required cursor "Expected arg")))
    (if (or (not (symbolp edebug-arg))
	    (edebug-lambda-list-keywordp edebug-arg))
      (edebug-no-match cursor "Bad argument:" edebug-arg))
    (edebug-move-cursor cursor)
    (setq edebug-def-args (cons edebug-arg edebug-def-args))
    (list edebug-arg)))

(defun edebug-match-def-form (cursor)
  ;; Like form but the form is wrapped in edebug-enter form.
  ;; The form is assumed to be executing outside of the function context.
  ;; This is a hack for now, since a def-form might execute inside as well.
  ;; Not to be used otherwise.
  (let ((edebug-inside-func nil))
    (list (edebug-make-enter-wrapper (list (edebug-form cursor))))))

(defun edebug-match-def-body (cursor)
  ;; Like body but body is wrapped in edebug-enter form.
  ;; The body is assumed to be executing inside of the function context.
  ;; Not to be used otherwise.
  (let ((edebug-inside-func t))
    (list (edebug-wrap-def-body (edebug-forms cursor)))))


;;;; Edebug Form Specs
;;; ==========================================================
;;; See cl-specs.el for common lisp specs.

;;;;* Spec for def-edebug-spec
;;; Out of date.

(defun edebug-spec-p (object)
  "Return non-nil if OBJECT is a symbol with an edebug-form-spec property."
  (and (symbolp object)
       (get object 'edebug-form-spec)))

(def-edebug-spec def-edebug-spec
  ;; Top level is different from lower levels.
  (&define :name edebug-spec name
	   &or "nil" edebug-spec-p "t" "0" (&rest edebug-spec)))

(def-edebug-spec edebug-spec-list
  ;; A list must have something in it, or it is nil, a symbolp
  ((edebug-spec . [&or nil edebug-spec])))

(def-edebug-spec edebug-spec
  (&or
   (vector &rest edebug-spec)		; matches a vector
   ("vector" &rest edebug-spec)		; matches a vector spec
   ("quote" symbolp)
   edebug-spec-list
   stringp
   [edebug-lambda-list-keywordp &rest edebug-spec]
   [keywordp gate edebug-spec]
   edebug-spec-p  ;; Including all the special ones e.g. form.
   symbolp;; a predicate
   ))


;;;* Emacs special forms and some functions.

;; quote expects only one argument, although it allows any number.
(def-edebug-spec quote sexp)

;; The standard defining forms.
(def-edebug-spec defconst defvar)
(def-edebug-spec defvar (symbolp &optional form stringp))

(def-edebug-spec defun
  (&define name lambda-list
	   [&optional stringp]
	   [&optional ("interactive" interactive)]
	   def-body))
;; FIXME? Isn't this missing the doc-string?  Cf defun.
(def-edebug-spec defmacro
  (&define name lambda-list [&optional ("declare" &rest sexp)] def-body))

(def-edebug-spec arglist lambda-list)  ;; deprecated - use lambda-list.

(def-edebug-spec lambda-list
  (([&rest arg]
    [&optional ["&optional" arg &rest arg]]
    &optional ["&rest" arg]
    )))

(def-edebug-spec interactive
  (&optional &or stringp def-form))

;; A function-form is for an argument that may be a function or a form.
;; This specially recognizes anonymous functions quoted with quote.
(def-edebug-spec function-form
  ;; form at the end could also handle "function",
  ;; but recognize it specially to avoid wrapping function forms.
  (&or ([&or "quote" "function"] &or symbolp lambda-expr) form))

;; function expects a symbol or a lambda or macro expression
;; A macro is allowed by Emacs.
(def-edebug-spec function (&or symbolp lambda-expr))

;; lambda is a macro in emacs 19.
(def-edebug-spec lambda (&define lambda-list
				 [&optional stringp]
				 [&optional ("interactive" interactive)]
				 def-body))

;; A macro expression is a lambda expression with "macro" prepended.
(def-edebug-spec macro (&define "lambda" lambda-list def-body))

;; (def-edebug-spec anonymous-form ((&or ["lambda" lambda] ["macro" macro])))

;; Standard functions that take function-forms arguments.
(def-edebug-spec mapcar (function-form form))
(def-edebug-spec mapconcat (function-form form form))
(def-edebug-spec mapatoms (function-form &optional form))
(def-edebug-spec apply (function-form &rest form))
(def-edebug-spec funcall (function-form &rest form))

;; FIXME?  The manual uses this form (maybe that's just for illustration?):
;; (def-edebug-spec let
;;   ((&rest &or symbolp (gate symbolp &optional form))
;;    body))
(def-edebug-spec let
  ((&rest &or (symbolp &optional form) symbolp)
   body))

(def-edebug-spec let* let)

(def-edebug-spec setq (&rest symbolp form))
(def-edebug-spec setq-default setq)

(def-edebug-spec cond (&rest (&rest form)))

(def-edebug-spec condition-case
  (symbolp
   form
   &rest ([&or symbolp (&rest symbolp)] body)))


(def-edebug-spec \` (backquote-form))

;; Supports quotes inside backquotes,
;; but only at the top level inside unquotes.
(def-edebug-spec backquote-form
  (&or
   ([&or "," ",@"] &or ("quote" backquote-form) form)
   ;; The simple version:
   ;;   (backquote-form &rest backquote-form)
   ;; doesn't handle (a . ,b).  The straightforward fix:
   ;;   (backquote-form . [&or nil backquote-form])
   ;; uses up too much stack space.
   ;; Note that `(foo . ,@bar) is not valid, so we don't need to handle it.
   (backquote-form [&rest [&not ","] backquote-form]
		   . [&or nil backquote-form])
   ;; If you use dotted forms in backquotes, replace the previous line
   ;; with the following.  This takes quite a bit more stack space, however.
   ;; (backquote-form . [&or nil backquote-form])
   (vector &rest backquote-form)
   sexp))

;; Special version of backquote that instruments backquoted forms
;; destined to be evaluated, usually as the result of a
;; macroexpansion.  Backquoted code can only have unquotes (, and ,@)
;; in places where list forms are allowed, and predicates. If the
;; backquote is used in a macro, unquoted code that come from
;; arguments must be instrumented, if at all, with def-form not def-body.

;; We could assume that all forms (not nested in other forms)
;; in arguments of macros should be def-forms, whether or not the macros
;; are defined with edebug-` but this would be expensive.

;; ,@ might have some problems.

(defalias 'edebug-\` '\`)  ;; same macro as regular backquote.
(def-edebug-spec edebug-\` (def-form))

;; Assume immediate quote in unquotes mean backquote at next higher level.
(def-edebug-spec \, (&or ("quote" edebug-\`) def-form))
(def-edebug-spec \,@ (&define  ;; so (,@ form) is never wrapped.
		     &or ("quote" edebug-\`) def-form))

;; New byte compiler.
(def-edebug-spec defsubst defun)
(def-edebug-spec dont-compile t)
(def-edebug-spec eval-when-compile t)
(def-edebug-spec eval-and-compile t)

(def-edebug-spec save-selected-window t)
(def-edebug-spec save-current-buffer t)
(def-edebug-spec delay-mode-hooks t)
(def-edebug-spec with-temp-file t)
(def-edebug-spec with-temp-message t)
(def-edebug-spec with-syntax-table t)
(def-edebug-spec push (form sexp))
(def-edebug-spec pop (sexp))

(def-edebug-spec 1value (form))
(def-edebug-spec noreturn (form))


;; Anything else?


;; Some miscellaneous specs for macros in public packages.
;; Send me yours.

;; advice.el by Hans Chalupsky (hans@cs.buffalo.edu)

(def-edebug-spec ad-dolist ((symbolp form &optional form) body))
(def-edebug-spec defadvice
  (&define name   ;; thing being advised.
	   (name  ;; class is [&or "before" "around" "after"
	          ;;               "activation" "deactivation"]
	    name  ;; name of advice
	    &rest sexp  ;; optional position and flags
	    )
	   [&optional stringp]
	   [&optional ("interactive" interactive)]
	   def-body))

(def-edebug-spec easy-menu-define (symbolp body))

(def-edebug-spec with-custom-print body)


;;; The debugger itself

(defvar edebug-active nil)  ;; Non-nil when edebug is active

;;; add minor-mode-alist entry
(or (assq 'edebug-active minor-mode-alist)
    (setq minor-mode-alist (cons (list 'edebug-active " *Debugging*")
				 minor-mode-alist)))

(defvar edebug-stack nil)
;; Stack of active functions evaluated via edebug.
;; Should be nil at the top level.

(defvar edebug-stack-depth -1)
;; Index of last edebug-stack item.

(defvar edebug-offset-indices nil)
;; Stack of offset indices of visited edebug sexps.
;; Should be nil at the top level.
;; Each function adds one cons.  Top is modified with setcar.


(defvar edebug-entered nil
  ;; Non-nil if edebug has already been entered at this recursive edit level.
  ;; This should stay nil at the top level.
  )

;; Should these be options?
(defconst edebug-debugger 'edebug
  ;; Name of function to use for debugging when error or quit occurs.
  ;; Set this to 'debug if you want to debug edebug.
  )


;; Dynamically bound variables, declared globally but left unbound.
(defvar edebug-function) ; the function being executed. change name!!
(defvar edebug-args) ; the arguments of the function
(defvar edebug-data) ; the edebug data for the function
(defvar edebug-value) ; the result of the expression
(defvar edebug-after-index)
(defvar edebug-def-mark) ; the mark for the definition
(defvar edebug-freq-count) ; the count of expression visits.
(defvar edebug-coverage) ; the coverage results of each expression of function.

(defvar edebug-buffer) ; which buffer the function is in.
(defvar edebug-result) ; the result of the function call returned by body
(defvar edebug-outside-executing-macro)
(defvar edebug-outside-defining-kbd-macro)

(defvar edebug-execution-mode 'step) ; Current edebug mode set by user.
(defvar edebug-next-execution-mode nil) ; Use once instead of initial mode.

(defvar edebug-outside-debug-on-error) ; the value of debug-on-error outside
(defvar edebug-outside-debug-on-quit) ; the value of debug-on-quit outside

(defvar edebug-outside-overriding-local-map)
(defvar edebug-outside-overriding-terminal-local-map)

(defvar edebug-outside-pre-command-hook)
(defvar edebug-outside-post-command-hook)

(defvar cl-lexical-debug)  ;; Defined in cl.el

;;; Handling signals

(defun edebug-signal (edebug-signal-name edebug-signal-data)
  "Signal an error.  Args are SIGNAL-NAME, and associated DATA.
A signal name is a symbol with an `error-conditions' property
that is a list of condition names.
A handler for any of those names will get to handle this signal.
The symbol `error' should always be one of them.

DATA should be a list.  Its elements are printed as part of the error message.
If the signal is handled, DATA is made available to the handler.
See `condition-case'.

This is the Edebug replacement for the standard `signal'.  It should
only be active while Edebug is.  It checks `debug-on-error' to see
whether it should call the debugger.  When execution is resumed, the
error is signaled again.
\n(fn SIGNAL-NAME DATA)"
  (if (and (listp debug-on-error) (memq edebug-signal-name debug-on-error))
      (edebug 'error (cons edebug-signal-name edebug-signal-data)))
  ;; If we reach here without another non-local exit, then send signal again.
  ;; i.e. the signal is not continuable, yet.
  ;; Avoid infinite recursion.
  (let ((signal-hook-function nil))
    (signal edebug-signal-name edebug-signal-data)))

;;; Entering Edebug

(defun edebug-enter (edebug-function edebug-args edebug-body)
  ;; Entering FUNC.  The arguments are ARGS, and the body is BODY.
  ;; Setup edebug variables and evaluate BODY.  This function is called
  ;; when a function evaluated with edebug-eval-top-level-form is entered.
  ;; Return the result of BODY.

  ;; Is this the first time we are entering edebug since
  ;; lower-level recursive-edit command?
  ;; More precisely, this tests whether Edebug is currently active.
  (if (not edebug-entered)
      (let ((edebug-entered t)
	    ;; Binding max-lisp-eval-depth here is OK,
	    ;; but not inside an unwind-protect.
	    ;; Doing it here also keeps it from growing too large.
	    (max-lisp-eval-depth (+ 100 max-lisp-eval-depth)) ; too much??
	    (max-specpdl-size (+ 200 max-specpdl-size))

	    (debugger edebug-debugger)  ; only while edebug is active.
	    (edebug-outside-debug-on-error debug-on-error)
	    (edebug-outside-debug-on-quit debug-on-quit)
	    ;; Binding these may not be the right thing to do.
	    ;; We want to allow the global values to be changed.
	    (debug-on-error (or debug-on-error edebug-on-error))
	    (debug-on-quit edebug-on-quit)

	    ;; Lexical bindings must be uncompiled for this to work.
	    (cl-lexical-debug t)

	    (edebug-outside-overriding-local-map overriding-local-map)
	    (edebug-outside-overriding-terminal-local-map
	     overriding-terminal-local-map)

	    ;; Save the outside value of executing macro.  (here??)
	    (edebug-outside-executing-macro executing-kbd-macro)
	    (edebug-outside-pre-command-hook
	     (edebug-var-status 'pre-command-hook))
	    (edebug-outside-post-command-hook
	     (edebug-var-status 'post-command-hook)))
	(unwind-protect
	    (let (;; Don't keep reading from an executing kbd macro
		  ;; within edebug unless edebug-continue-kbd-macro is
		  ;; non-nil.  Again, local binding may not be best.
		  (executing-kbd-macro
		   (if edebug-continue-kbd-macro executing-kbd-macro))

		  ;; Don't get confused by the user's keymap changes.
		  (overriding-local-map nil)
		  (overriding-terminal-local-map nil)

		  (signal-hook-function 'edebug-signal)

		  ;; Disable command hooks.  This is essential when
		  ;; a hook function is instrumented - to avoid infinite loop.
		  ;; This may be more than we need, however.
		  (pre-command-hook nil)
		  (post-command-hook nil))
	      (setq edebug-execution-mode (or edebug-next-execution-mode
					      edebug-initial-mode
					      edebug-execution-mode)
		    edebug-next-execution-mode nil)
	      (edebug-enter edebug-function edebug-args edebug-body))
	  ;; Reset global variables in case outside value was changed.
	  (setq executing-kbd-macro edebug-outside-executing-macro)
	  (edebug-restore-status
	   'post-command-hook edebug-outside-post-command-hook)
	  (edebug-restore-status
	   'pre-command-hook edebug-outside-pre-command-hook)))

    (let* ((edebug-data (get edebug-function 'edebug))
	   (edebug-def-mark (car edebug-data)) ; mark at def start
	   (edebug-freq-count (get edebug-function 'edebug-freq-count))
	   (edebug-coverage (get edebug-function 'edebug-coverage))
	   (edebug-buffer (marker-buffer edebug-def-mark))

	   (edebug-stack (cons edebug-function edebug-stack))
	   (edebug-offset-indices (cons 0 edebug-offset-indices))
	   )
      (if (get edebug-function 'edebug-on-entry)
	  (progn
	    (setq edebug-execution-mode 'step)
	    (if (eq (get edebug-function 'edebug-on-entry) 'temp)
		(put edebug-function 'edebug-on-entry nil))))
      (if edebug-trace
	  (edebug-enter-trace edebug-body)
	(funcall edebug-body))
      )))

(defun edebug-var-status (var)
  "Return a cons cell describing the status of VAR's current binding.
The purpose of this function is so you can properly undo
subsequent changes to the same binding, by passing the status
cons cell to `edebug-restore-status'.  The status cons cell
has the form (LOCUS . VALUE), where LOCUS can be a buffer
\(for a buffer-local binding), a frame (for a frame-local binding),
or nil (if the default binding is current)."
  (cons (variable-binding-locus var)
	(symbol-value var)))

(defun edebug-restore-status (var status)
  "Reset VAR based on STATUS.
STATUS should be a list returned by `edebug-var-status'."
  (let ((locus (car status))
	(value (cdr status)))
    (cond ((bufferp locus)
	   (if (buffer-live-p locus)
	       (with-current-buffer locus
		 (set var value))))
	  ((framep locus)
	   (modify-frame-parameters locus (list (cons var value))))
	  (t
	   (set var value)))))

(defun edebug-enter-trace (edebug-body)
  (let ((edebug-stack-depth (1+ edebug-stack-depth))
	edebug-result)
    (edebug-print-trace-before
     (format "%s args: %s" edebug-function edebug-args))
    (prog1 (setq edebug-result (funcall edebug-body))
      (edebug-print-trace-after
       (format "%s result: %s" edebug-function edebug-result)))))

(def-edebug-spec edebug-tracing (form body))

(defmacro edebug-tracing (msg &rest body)
  "Print MSG in *edebug-trace* before and after evaluating BODY.
The result of BODY is also printed."
  `(let ((edebug-stack-depth (1+ edebug-stack-depth))
	 edebug-result)
     (edebug-print-trace-before ,msg)
     (prog1 (setq edebug-result (progn ,@body))
       (edebug-print-trace-after
	(format "%s result: %s" ,msg edebug-result)))))

(defun edebug-print-trace-before (msg)
  "Function called to print trace info before expression evaluation.
MSG is printed after `::::{ '."
  (edebug-trace-display
   edebug-trace-buffer "%s{ %s" (make-string edebug-stack-depth ?\:) msg))

(defun edebug-print-trace-after (msg)
  "Function called to print trace info after expression evaluation.
MSG is printed after `::::} '."
  (edebug-trace-display
   edebug-trace-buffer "%s} %s" (make-string edebug-stack-depth ?\:) msg))



(defun edebug-slow-before (edebug-before-index)
  (unless edebug-active
    ;; Debug current function given BEFORE position.
    ;; Called from functions compiled with edebug-eval-top-level-form.
    ;; Return the before index.
    (setcar edebug-offset-indices edebug-before-index)

    ;; Increment frequency count
    (aset edebug-freq-count edebug-before-index
	  (1+ (aref edebug-freq-count edebug-before-index)))

    (if (or (not (memq edebug-execution-mode '(Go-nonstop next)))
	    (edebug-input-pending-p))
	(edebug-debugger edebug-before-index 'before nil)))
  edebug-before-index)

(defun edebug-fast-before (edebug-before-index)
  ;; Do nothing.
  )

(defun edebug-slow-after (edebug-before-index edebug-after-index edebug-value)
  (if edebug-active
      edebug-value
    ;; Debug current function given AFTER position and VALUE.
    ;; Called from functions compiled with edebug-eval-top-level-form.
    ;; Return VALUE.
    (setcar edebug-offset-indices edebug-after-index)

    ;; Increment frequency count
    (aset edebug-freq-count edebug-after-index
	  (1+ (aref edebug-freq-count edebug-after-index)))
    (if edebug-test-coverage (edebug-update-coverage))

    (if (and (eq edebug-execution-mode 'Go-nonstop)
	     (not (edebug-input-pending-p)))
	;; Just return result.
	edebug-value
      (edebug-debugger edebug-after-index 'after edebug-value)
      )))

(defun edebug-fast-after (edebug-before-index edebug-after-index edebug-value)
  ;; Do nothing but return the value.
  edebug-value)

(defun edebug-run-slow ()
  (defalias 'edebug-before 'edebug-slow-before)
  (defalias 'edebug-after 'edebug-slow-after))

;; This is not used, yet.
(defun edebug-run-fast ()
  (defalias 'edebug-before 'edebug-fast-before)
  (defalias 'edebug-after 'edebug-fast-after))

(edebug-run-slow)


(defun edebug-update-coverage ()
  (let ((old-result (aref edebug-coverage edebug-after-index)))
    (cond
     ((eq 'ok-coverage old-result))
     ((eq 'unknown old-result)
      (aset edebug-coverage edebug-after-index edebug-value))
     ;; Test if a different result.
     ((not (eq edebug-value old-result))
      (aset edebug-coverage edebug-after-index 'ok-coverage)))))


;; Dynamically declared unbound variables.
(defvar edebug-arg-mode)  ; the mode, either before, after, or error
(defvar edebug-breakpoints)
(defvar edebug-break-data) ; break data for current function.
(defvar edebug-break) ; whether a break occurred.
(defvar edebug-global-break) ; whether a global break occurred.
(defvar edebug-break-condition) ; whether the breakpoint is conditional.

(defvar edebug-break-result nil)
(defvar edebug-global-break-result nil)


(defun edebug-debugger (edebug-offset-index edebug-arg-mode edebug-value)
  (if inhibit-redisplay
      ;; Don't really try to enter edebug within an eval from redisplay.
      edebug-value
    ;; Check breakpoints and pending input.
    ;; If edebug display should be updated, call edebug-display.
    ;; Return edebug-value.
    (let* ( ;; This needs to be here since breakpoints may be changed.
	   (edebug-breakpoints (car (cdr edebug-data)))	; list of breakpoints
	   (edebug-break-data (assq edebug-offset-index edebug-breakpoints))
	   (edebug-break-condition (car (cdr edebug-break-data)))
	   (edebug-global-break
	    (if edebug-global-break-condition
		(condition-case nil
		    (setq edebug-global-break-result
                          ;; FIXME: lexbind.
			  (eval edebug-global-break-condition))
		  (error nil))))
	   (edebug-break))

;;;    (edebug-trace "exp: %s" edebug-value)
      ;; Test whether we should break.
      (setq edebug-break
	    (or edebug-global-break
		(and edebug-break-data
		     (or (not edebug-break-condition)
			 (setq edebug-break-result
                               ;; FIXME: lexbind.
			       (eval edebug-break-condition))))))
      (if (and edebug-break
	       (nth 2 edebug-break-data)) ; is it temporary?
	  ;; Delete the breakpoint.
	  (setcdr edebug-data
		  (cons (delq edebug-break-data edebug-breakpoints)
			(cdr (cdr edebug-data)))))

      ;; Display if mode is not go, continue, or Continue-fast
      ;; or break, or input is pending,
      (if (or (not (memq edebug-execution-mode '(go continue Continue-fast)))
	      edebug-break
	      (edebug-input-pending-p))
	  (edebug-display))		; <--------------- display

      edebug-value
      )))


;; window-start now stored with each function.
;;(defvar edebug-window-start nil)
;; Remember where each buffers' window starts between edebug calls.
;; This is to avoid spurious recentering.
;; Does this still need to be buffer-local??
;;(setq-default edebug-window-start nil)
;;(make-variable-buffer-local 'edebug-window-start)


;; Dynamically declared unbound vars
(defvar edebug-point) ; the point in edebug buffer
(defvar edebug-outside-buffer) ; the current-buffer outside of edebug
(defvar edebug-outside-point) ; the point outside of edebug
(defvar edebug-outside-mark) ; the mark outside of edebug
(defvar edebug-window-data)  ; window and window-start for current function
(defvar edebug-outside-windows) ; outside window configuration
(defvar edebug-eval-buffer) ; for the evaluation list.
(defvar edebug-outside-o-a-p) ; outside overlay-arrow-position
(defvar edebug-outside-o-a-s) ; outside overlay-arrow-string
(defvar edebug-outside-c-i-e-a) ; outside cursor-in-echo-area
(defvar edebug-outside-d-c-i-n-s-w) ; outside default-cursor-in-non-selected-windows

(defvar edebug-eval-list nil) ;; List of expressions to evaluate.

(defvar edebug-previous-result nil) ;; Last result returned.

;; Emacs 19 adds an arg to mark and mark-marker.
(defalias 'edebug-mark-marker 'mark-marker)


(defun edebug-display ()
  (unless (marker-position edebug-def-mark)
    ;; The buffer holding the source has been killed.
    ;; Let's at least show a backtrace so the user can figure out
    ;; which function we're talking about.
    (debug))
  ;; Setup windows for edebug, determine mode, maybe enter recursive-edit.
  ;; Uses local variables of edebug-enter, edebug-before, edebug-after
  ;; and edebug-debugger.
  (let ((edebug-active t)		; for minor mode alist
	(edebug-with-timeout-suspend (with-timeout-suspend))
	edebug-stop			; should we enter recursive-edit
	(edebug-point (+ edebug-def-mark
			 (aref (nth 2 edebug-data) edebug-offset-index)))
	edebug-buffer-outside-point     ; current point in edebug-buffer
	;; window displaying edebug-buffer
	(edebug-window-data (nth 3 edebug-data))
	(edebug-outside-window (selected-window))
	(edebug-outside-buffer (current-buffer))
	(edebug-outside-point (point))
 	(edebug-outside-mark (edebug-mark))
	(edebug-outside-unread-command-events unread-command-events)
	edebug-outside-windows		; window or screen configuration
	edebug-buffer-points

	edebug-eval-buffer		; declared here so we can kill it below
	(edebug-eval-result-list (and edebug-eval-list
				      (edebug-eval-result-list)))
	edebug-trace-window
	edebug-trace-window-start

	(edebug-outside-o-a-p overlay-arrow-position)
	(edebug-outside-o-a-s overlay-arrow-string)
	(edebug-outside-c-i-e-a cursor-in-echo-area)
	(edebug-outside-d-c-i-n-s-w
         (default-value 'cursor-in-non-selected-windows)))
    (unwind-protect
	(let ((overlay-arrow-position overlay-arrow-position)
	      (overlay-arrow-string overlay-arrow-string)
	      (cursor-in-echo-area nil)
	      (unread-command-events unread-command-events)
	      ;; any others??
	      )
          (setq-default cursor-in-non-selected-windows t)
	  (if (not (buffer-name edebug-buffer))
	      (let ((debug-on-error nil))
		(error "Buffer defining %s not found" edebug-function)))

	  (if (eq 'after edebug-arg-mode)
	      ;; Compute result string now before windows are modified.
	      (edebug-compute-previous-result edebug-value))

	  (if edebug-save-windows
	      ;; Save windows now before we modify them.
	      (setq edebug-outside-windows
		    (edebug-current-windows edebug-save-windows)))

	  (if edebug-save-displayed-buffer-points
	      (setq edebug-buffer-points (edebug-get-displayed-buffer-points)))

	  ;; First move the edebug buffer point to edebug-point
	  ;; so that window start doesn't get changed when we display it.
	  ;; I don't know if this is going to help.
	  ;;(set-buffer edebug-buffer)
	  ;;(goto-char edebug-point)

	  ;; If edebug-buffer is not currently displayed,
	  ;; first find a window for it.
	  (edebug-pop-to-buffer edebug-buffer (car edebug-window-data))
	  (setcar edebug-window-data (selected-window))

	  ;; Now display eval list, if any.
	  ;; This is done after the pop to edebug-buffer
	  ;; so that buffer-window correspondence is correct after quitting.
	  (edebug-eval-display edebug-eval-result-list)
	  ;; The evaluation list better not have deleted edebug-window-data.
	  (select-window (car edebug-window-data))
	  (set-buffer edebug-buffer)

	  (setq edebug-buffer-outside-point (point))
	  (goto-char edebug-point)

	  (if (eq 'before edebug-arg-mode)
	      ;; Check whether positions are up-to-date.
	      ;; This assumes point is never before symbol.
	      (if (not (memq (following-char) '(?\( ?\# ?\` )))
		  (let ((debug-on-error nil))
		    (error "Source has changed - reevaluate definition of %s"
			   edebug-function)
		    )))

	  (setcdr edebug-window-data
		  (edebug-adjust-window (cdr edebug-window-data)))

	  ;; Test if there is input, not including keyboard macros.
	  (if (edebug-input-pending-p)
	      (progn
		(setq edebug-execution-mode 'step
		      edebug-stop t)
		(edebug-stop)
		;;	    (discard-input)		; is this unfriendly??
		))
	  ;; Now display arrow based on mode.
	  (edebug-overlay-arrow)

	  (cond
	   ((eq 'error edebug-arg-mode)
	    ;; Display error message
	    (setq edebug-execution-mode 'step)
	    (edebug-overlay-arrow)
	    (beep)
	    (if (eq 'quit (car edebug-value))
		(message "Quit")
	      (edebug-report-error edebug-value)))
	   (edebug-break
	    (cond
	     (edebug-global-break
	      (message "Global Break: %s => %s"
		       edebug-global-break-condition
		       edebug-global-break-result))
	     (edebug-break-condition
	      (message "Break: %s => %s"
		       edebug-break-condition
		       edebug-break-result))
	     ((not (eq edebug-execution-mode 'Continue-fast))
	      (message "Break"))
	     (t)))

	   (t (message "")))

	  (setq unread-command-events nil)
	  (if (eq 'after edebug-arg-mode)
	      (progn
		;; Display result of previous evaluation.
		(if (and edebug-break
			 (not (eq edebug-execution-mode 'Continue-fast)))
                    (edebug-sit-for edebug-sit-for-seconds)) ; Show message.
		(edebug-previous-result)))

	  (cond
	   (edebug-break
	    (cond
	     ((eq edebug-execution-mode 'continue)
              (edebug-sit-for edebug-sit-for-seconds))
	     ((eq edebug-execution-mode 'Continue-fast) (edebug-sit-for 0))
	     (t (setq edebug-stop t))))
	   ;; not edebug-break
	   ((eq edebug-execution-mode 'trace)
	    (edebug-sit-for edebug-sit-for-seconds)) ; Force update and pause.
	   ((eq edebug-execution-mode 'Trace-fast)
	    (edebug-sit-for 0)))		; Force update and continue.

	  (unwind-protect
	      (if (or edebug-stop
		      (memq edebug-execution-mode '(step next))
		      (eq edebug-arg-mode 'error))
		  (progn
		    ;; (setq edebug-execution-mode 'step)
		    ;; (edebug-overlay-arrow)	; This doesn't always show up.
		    (edebug-recursive-edit))) ; <---------- Recursive edit

	    ;; Reset the edebug-window-data to whatever it is now.
	    (let ((window (if (eq (window-buffer) edebug-buffer)
			      (selected-window)
			    (edebug-get-buffer-window edebug-buffer))))
	      ;; Remember window-start for edebug-buffer, if still displayed.
	      (if window
		  (progn
		    (setcar edebug-window-data window)
		    (setcdr edebug-window-data (window-start window)))))

	    ;; Save trace window point before restoring outside windows.
	    ;; Could generalize this for other buffers.
	    (setq edebug-trace-window (get-buffer-window edebug-trace-buffer))
	    (if edebug-trace-window
		(setq edebug-trace-window-start
		      (and edebug-trace-window
			   (window-start edebug-trace-window))))

	    ;; Restore windows before continuing.
	    (if edebug-save-windows
		(progn
		  (edebug-set-windows edebug-outside-windows)

		  ;; Restore displayed buffer points.
		  ;; Needed even if restoring windows because
		  ;; window-points are not restored. (should they be??)
		  (if edebug-save-displayed-buffer-points
		      (edebug-set-buffer-points edebug-buffer-points))

		  ;; Unrestore trace window's window-point.
		  (if edebug-trace-window
		      (set-window-start edebug-trace-window
					edebug-trace-window-start))

		  ;; Unrestore edebug-buffer's window-start, if displayed.
		  (let ((window (car edebug-window-data)))
		    (if (and (edebug-window-live-p window)
			     (eq (window-buffer) edebug-buffer))
			(progn
			  (set-window-start window (cdr edebug-window-data)
					    'no-force)
			  ;; Unrestore edebug-buffer's window-point.
			  ;; Needed in addition to setting the buffer point
			  ;; - otherwise quitting doesn't leave point as is.
			  ;; But this causes point to not be restored at times.
			  ;; Also, it may not be a visible window.
			  ;; (set-window-point window edebug-point)
			  )))

		  ;; Unrestore edebug-buffer's point.   Rerestored below.
		  ;;  (goto-char edebug-point) ;; in edebug-buffer
		  )
	      ;; Since we may be in a save-excursion, in case of quit,
	      ;; reselect the outside window only.
	      ;; Only needed if we are not recovering windows??
	      (if (edebug-window-live-p edebug-outside-window)
		  (select-window edebug-outside-window))
	      )				; if edebug-save-windows

	    ;; Restore current buffer always, in case application needs it.
	    (if (buffer-name edebug-outside-buffer)
		(set-buffer edebug-outside-buffer))
	    ;; Restore point, and mark.
	    ;; Needed even if restoring windows because
	    ;; that doesn't restore point and mark in the current buffer.
	    ;; But don't restore point if edebug-buffer is current buffer.
	    (if (not (eq edebug-buffer edebug-outside-buffer))
		(goto-char edebug-outside-point))
	    (if (marker-buffer (edebug-mark-marker))
		;; Does zmacs-regions need to be nil while doing set-marker?
		(set-marker (edebug-mark-marker) edebug-outside-mark))
	    )				; unwind-protect
	  ;; None of the following is done if quit or signal occurs.

	  ;; Restore edebug-buffer's outside point.
	  ;;    (edebug-trace "restore edebug-buffer point: %s"
	  ;;		  edebug-buffer-outside-point)
	  (with-current-buffer edebug-buffer
	    (goto-char edebug-buffer-outside-point))
	  ;; ... nothing more.
	  )
      (with-timeout-unsuspend edebug-with-timeout-suspend)
      ;; Reset global variables to outside values in case they were changed.
      (setq
       unread-command-events edebug-outside-unread-command-events
       overlay-arrow-position edebug-outside-o-a-p
       overlay-arrow-string edebug-outside-o-a-s
       cursor-in-echo-area edebug-outside-c-i-e-a)
      (setq-default cursor-in-non-selected-windows edebug-outside-d-c-i-n-s-w)
      )))


(defvar edebug-number-of-recursions 0)
;; Number of recursive edits started by edebug.
;; Should be 0 at the top level.

(defvar edebug-recursion-depth 0)
;; Value of recursion-depth when edebug was called.

;; Dynamically declared unbound vars
(defvar edebug-outside-match-data) ; match data outside of edebug
(defvar edebug-backtrace-buffer) ; each recursive edit gets its own
(defvar edebug-inside-windows)
(defvar edebug-interactive-p)

(defvar edebug-outside-map)
(defvar edebug-outside-standard-output)
(defvar edebug-outside-standard-input)
(defvar edebug-outside-current-prefix-arg)
(defvar edebug-outside-last-command)
(defvar edebug-outside-this-command)

;; Note: here we have defvars for variables that are
;; built-in in certain versions.
;; Each defvar makes a difference
;; in versions where the variable is *not* built-in.

;; Emacs 18  FIXME
(defvar edebug-outside-unread-command-char)

;; Emacs 19.
(defvar edebug-outside-last-command-event)
(defvar edebug-outside-unread-command-events)
(defvar edebug-outside-last-input-event)
(defvar edebug-outside-last-event-frame)
(defvar edebug-outside-last-nonmenu-event)
(defvar edebug-outside-track-mouse)

;; Disable byte compiler warnings about unread-command-char and -event
;; (maybe works with byte-compile-version 2.22 at least)
(defvar edebug-unread-command-char-warning)
(defvar edebug-unread-command-event-warning)
(eval-when-compile			; FIXME
  (setq edebug-unread-command-char-warning
	(get 'unread-command-char 'byte-obsolete-variable))
  (put 'unread-command-char 'byte-obsolete-variable nil))

(defun edebug-recursive-edit ()
  ;; Start up a recursive edit inside of edebug.
  ;; The current buffer is the edebug-buffer, which is put into edebug-mode.
  ;; Assume that none of the variables below are buffer-local.
  (let ((edebug-buffer-read-only buffer-read-only)
	;; match-data must be done in the outside buffer
	(edebug-outside-match-data
	 (with-current-buffer edebug-outside-buffer ; in case match buffer different
	   (match-data)))

	;;(edebug-number-of-recursions (1+ edebug-number-of-recursions))
	(edebug-recursion-depth (recursion-depth))
	edebug-entered			; bind locally to nil
	(edebug-interactive-p nil)      ; again non-interactive
	edebug-backtrace-buffer		; each recursive edit gets its own
	;; The window configuration may be saved and restored
	;; during a recursive-edit
	edebug-inside-windows

	(edebug-outside-map (current-local-map))

	(edebug-outside-standard-output standard-output)
	(edebug-outside-standard-input standard-input)
	(edebug-outside-defining-kbd-macro defining-kbd-macro)

	(edebug-outside-last-command last-command)
	(edebug-outside-this-command this-command)

	(edebug-outside-unread-command-char unread-command-char) ; FIXME
	(edebug-outside-current-prefix-arg current-prefix-arg)

	(edebug-outside-last-input-event last-input-event)
	(edebug-outside-last-command-event last-command-event)
	(edebug-outside-last-event-frame last-event-frame)
	(edebug-outside-last-nonmenu-event last-nonmenu-event)
	(edebug-outside-track-mouse track-mouse)
	)

    (unwind-protect
	(let (
	      ;; Declare global values local but using the same global value.
	      ;; We could set these to the values for previous edebug call.
	      (last-command last-command)
	      (this-command this-command)

	      ;; Assume no edebug command sets unread-command-char.
	      (unread-command-char -1)
	      (current-prefix-arg nil)

	      ;; More for Emacs 19
	      (last-input-event nil)
	      (last-command-event nil)
	      (last-event-frame nil)
	      (last-nonmenu-event nil)
	      (track-mouse nil)

	      ;; Bind again to outside values.
	      (debug-on-error edebug-outside-debug-on-error)
	      (debug-on-quit edebug-outside-debug-on-quit)

	      ;; Don't keep defining a kbd macro.
	      (defining-kbd-macro
		(if edebug-continue-kbd-macro defining-kbd-macro))

	      ;; others??
	      )

	  (if (and (eq edebug-execution-mode 'go)
		   (not (memq edebug-arg-mode '(after error))))
	      (message "Break"))

	  (setq buffer-read-only t)
	  (setq signal-hook-function nil)

	  (edebug-mode)
	  (unwind-protect
	      (recursive-edit)		;  <<<<<<<<<< Recursive edit

	    ;; Do the following, even if quit occurs.
	    (setq signal-hook-function 'edebug-signal)
	    (if edebug-backtrace-buffer
		(kill-buffer edebug-backtrace-buffer))
	    ;; Could be an option to keep eval display up.
	    (if edebug-eval-buffer (kill-buffer edebug-eval-buffer))

	    ;; Remember selected-window after recursive-edit.
	    ;;      (setq edebug-inside-window (selected-window))

	    (set-match-data edebug-outside-match-data)

	    ;; Recursive edit may have changed buffers,
	    ;; so set it back before exiting let.
	    (if (buffer-name edebug-buffer) ; if it still exists
		(progn
		  (set-buffer edebug-buffer)
		  (if (memq edebug-execution-mode '(go Go-nonstop))
		      (edebug-overlay-arrow))
		  (setq buffer-read-only edebug-buffer-read-only)
		  (use-local-map edebug-outside-map)
		  (remove-hook 'kill-buffer-hook 'edebug-kill-buffer t)
		  )
	      ;; gotta have a buffer to let its buffer local variables be set
	      (get-buffer-create " bogus edebug buffer"))
	    ));; inner let

      ;; Reset global vars to outside values, in case they have been changed.
      (setq
       last-command-event edebug-outside-last-command-event
       last-command edebug-outside-last-command
       this-command edebug-outside-this-command
       unread-command-char edebug-outside-unread-command-char
       current-prefix-arg edebug-outside-current-prefix-arg
       last-input-event edebug-outside-last-input-event
       last-event-frame edebug-outside-last-event-frame
       last-nonmenu-event edebug-outside-last-nonmenu-event
       track-mouse edebug-outside-track-mouse

       standard-output edebug-outside-standard-output
       standard-input edebug-outside-standard-input
       defining-kbd-macro edebug-outside-defining-kbd-macro
       ))
    ))


;;; Display related functions

(defun edebug-adjust-window (old-start)
  ;; If pos is not visible, adjust current window to fit following context.
;;;  (message "window: %s old-start: %s window-start: %s pos: %s"
;;;	   (selected-window) old-start (window-start) (point)) (sit-for 5)
  (if (not (pos-visible-in-window-p))
      (progn
	;; First try old-start
	(if old-start
	    (set-window-start (selected-window) old-start))
	(if (not (pos-visible-in-window-p))
	    (progn
;;	(message "resetting window start") (sit-for 2)
	(set-window-start
	 (selected-window)
	 (save-excursion
	   (forward-line
	    (if (< (point) (window-start)) -1	; one line before if in back
	      (- (/ (window-height) 2)) ; center the line moving forward
	      ))
	   (beginning-of-line)
	   (point)))))))
  (window-start))



(defconst edebug-arrow-alist
  '((Continue-fast . "=")
    (Trace-fast . "-")
    (continue . ">")
    (trace . "->")
    (step . "=>")
    (next . "=>")
    (go . "<>")
    (Go-nonstop . "..")  ; not used
    )
  "Association list of arrows for each edebug mode.")

(defun edebug-overlay-arrow ()
  ;; Set up the overlay arrow at beginning-of-line in current buffer.
  ;; The arrow string is derived from edebug-arrow-alist and
  ;; edebug-execution-mode.
  (let ((pos (line-beginning-position)))
    (setq overlay-arrow-string
	  (cdr (assq edebug-execution-mode edebug-arrow-alist)))
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))


(defun edebug-toggle-save-all-windows ()
  "Toggle the saving and restoring of all windows.
Also, each time you toggle it on, the inside and outside window
configurations become the same as the current configuration."
  (interactive)
  (setq edebug-save-windows (not edebug-save-windows))
  (if edebug-save-windows
      (setq edebug-inside-windows
	    (setq edebug-outside-windows
		  (edebug-current-windows
		   edebug-save-windows))))
  (message "Window saving is %s for all windows."
	   (if edebug-save-windows "on" "off")))

(defmacro edebug-changing-windows (&rest body)
  `(let ((window (selected-window)))
     (setq edebug-inside-windows (edebug-current-windows t))
     (edebug-set-windows edebug-outside-windows)
     ,@body;; Code to change edebug-save-windows
     (setq edebug-outside-windows (edebug-current-windows
				   edebug-save-windows))
     ;; Problem: what about outside windows that are deleted inside?
     (edebug-set-windows edebug-inside-windows)))

(defun edebug-toggle-save-selected-window ()
  "Toggle the saving and restoring of the selected window.
Also, each time you toggle it on, the inside and outside window
configurations become the same as the current configuration."
  (interactive)
  (cond
   ((eq t edebug-save-windows)
    ;; Save all outside windows except the selected one.
    ;; Remove (selected-window) from outside-windows.
    (edebug-changing-windows
     (setq edebug-save-windows (delq window (edebug-window-list)))))

   ((memq (selected-window) edebug-save-windows)
    (setq edebug-outside-windows
	  (delq (assq (selected-window) edebug-outside-windows)
		edebug-outside-windows))
    (setq edebug-save-windows
	  (delq (selected-window) edebug-save-windows)))
   (t					; Save a new window.
    (edebug-changing-windows
     (setq edebug-save-windows (cons window edebug-save-windows)))))

  (message "Window saving is %s for %s."
	   (if (memq (selected-window) edebug-save-windows)
	       "on" "off")
	   (selected-window)))

(defun edebug-toggle-save-windows (arg)
  "Toggle the saving and restoring of windows.
With prefix, toggle for just the selected window.
Otherwise, toggle for all windows."
  (interactive "P")
  (if arg
      (edebug-toggle-save-selected-window)
    (edebug-toggle-save-all-windows)))


(defun edebug-where ()
  "Show the debug windows and where we stopped in the program."
  (interactive)
  (if (not edebug-active)
      (error "Edebug is not active"))
  ;; Restore the window configuration to what it last was inside.
  ;; But it is not always set.   - experiment
  ;;(if edebug-inside-windows
  ;;  (edebug-set-windows edebug-inside-windows))
  (edebug-pop-to-buffer edebug-buffer)
  (goto-char edebug-point))

(defun edebug-view-outside ()
  "Change to the outside window configuration.
Use `edebug-where' to return."
  (interactive)
  (if (not edebug-active)
      (error "Edebug is not active"))
  (setq edebug-inside-windows
	(edebug-current-windows edebug-save-windows))
  (edebug-set-windows edebug-outside-windows)
  (goto-char edebug-outside-point)
  (message "Window configuration outside of Edebug.  Return with %s"
	   (substitute-command-keys "\\<global-map>\\[edebug-where]")))


(defun edebug-bounce-point (arg)
  "Bounce the point in the outside current buffer.
If prefix argument ARG is supplied, sit for that many seconds
before returning.  The default is one second."
  (interactive "p")
  (if (not edebug-active)
      (error "Edebug is not active"))
  (save-excursion
    ;; If the buffer's currently displayed, avoid set-window-configuration.
    (save-window-excursion
      (edebug-pop-to-buffer edebug-outside-buffer)
      (goto-char edebug-outside-point)
      (message "Current buffer: %s Point: %s Mark: %s"
	       (current-buffer) (point)
	       (if (marker-buffer (edebug-mark-marker))
		   (marker-position (edebug-mark-marker)) "<not set>"))
      (edebug-sit-for arg)
      (edebug-pop-to-buffer edebug-buffer (car edebug-window-data)))))


;; Joe Wells, here is a start at your idea of adding a buffer to the internal
;; display list.  Still need to use this list in edebug-display.

'(defvar edebug-display-buffer-list nil
  "List of buffers that edebug will display when it is active.")

'(defun edebug-display-buffer (buffer)
  "Toggle display of a buffer inside of edebug."
  (interactive "bBuffer: ")
  (let ((already-displaying (memq buffer edebug-display-buffer-list)))
    (setq edebug-display-buffer-list
	  (if already-displaying
	      (delq buffer edebug-display-buffer-list)
	    (cons buffer edebug-display-buffer-list)))
    (message "Displaying %s %s" buffer
	     (if already-displaying "off" "on"))))

;;; Breakpoint related functions

(defun edebug-find-stop-point ()
  ;; Return (function . index) of the nearest edebug stop point.
  (let* ((edebug-def-name (edebug-form-data-symbol))
	 (edebug-data
	   (let ((data (get edebug-def-name 'edebug)))
	     (if (or (null data) (markerp data))
		 (error "%s is not instrumented for Edebug" edebug-def-name))
	     data))  ; we could do it automatically, if data is a marker.
	 ;; pull out parts of edebug-data.
	 (edebug-def-mark (car edebug-data))
	 ;; (edebug-breakpoints (car (cdr edebug-data)))

	 (offset-vector (nth 2 edebug-data))
	 (offset (- (save-excursion
		      (if (looking-at "[ \t]")
			  ;; skip backwards until non-whitespace, or bol
			  (skip-chars-backward " \t"))
		      (point))
		    edebug-def-mark))
	 len i)
    ;; the offsets are in order so we can do a linear search
    (setq len (length offset-vector))
    (setq i 0)
    (while (and (< i len) (> offset (aref offset-vector i)))
      (setq i (1+ i)))
    (if (and (< i len)
	     (<= offset (aref offset-vector i)))
	;; return the relevant info
	(cons edebug-def-name i)
      (message "Point is not on an expression in %s."
	       edebug-def-name)
      )))


(defun edebug-next-breakpoint ()
  "Move point to the next breakpoint, or first if none past point."
  (interactive)
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((edebug-def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get edebug-def-name 'edebug))

	       ;; pull out parts of edebug-data
	       (edebug-def-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (nth 2 edebug-data))
	       breakpoint)
	  (if (not edebug-breakpoints)
	      (message "No breakpoints in this function.")
	    (let ((breaks edebug-breakpoints))
	      (while (and breaks
			  (<= (car (car breaks)) index))
		(setq breaks (cdr breaks)))
	      (setq breakpoint
		    (if breaks
			(car breaks)
		      ;; goto the first breakpoint
		      (car edebug-breakpoints)))
	      (goto-char (+ edebug-def-mark
			    (aref offset-vector (car breakpoint))))

	      (message "%s"
		       (concat (if (nth 2 breakpoint)
				   "Temporary " "")
			       (if (car (cdr breakpoint))
				   (format "Condition: %s"
					   (edebug-safe-prin1-to-string
					    (car (cdr breakpoint))))
				 "")))
	      ))))))


(defun edebug-modify-breakpoint (flag &optional condition temporary)
  "Modify the breakpoint for the form at point or after it.
Set it if FLAG is non-nil, clear it otherwise.  Then move to that point.
If CONDITION or TEMPORARY are non-nil, add those attributes to
the breakpoint."
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((edebug-def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get edebug-def-name 'edebug))

	       ;; pull out parts of edebug-data
	       (edebug-def-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (nth 2 edebug-data))
	       present)
	  ;; delete it either way
	  (setq present (assq index edebug-breakpoints))
	  (setq edebug-breakpoints (delq present edebug-breakpoints))
	  (if flag
	      (progn
		;; add it to the list and resort
		(setq edebug-breakpoints
		      (edebug-sort-alist
		       (cons
			(list index condition temporary)
			edebug-breakpoints) '<))
		(if condition
		    (message "Breakpoint set in %s with condition: %s"
			     edebug-def-name condition)
		  (message "Breakpoint set in %s" edebug-def-name)))
	    (if present
		(message "Breakpoint unset in %s" edebug-def-name)
	      (message "No breakpoint here")))

	  (setcar (cdr edebug-data) edebug-breakpoints)
	  (goto-char (+ edebug-def-mark (aref offset-vector index)))
	  ))))

(defun edebug-set-breakpoint (arg)
  "Set the breakpoint of nearest sexp.
With prefix argument, make it a temporary breakpoint."
  (interactive "P")
  (edebug-modify-breakpoint t nil arg))

(defun edebug-unset-breakpoint ()
  "Clear the breakpoint of nearest sexp."
  (interactive)
  (edebug-modify-breakpoint nil))


(defun edebug-set-global-break-condition (expression)
  "Set `edebug-global-break-condition' to EXPRESSION."
  (interactive
   (list
    (let ((initial (and edebug-global-break-condition
			(format "%s" edebug-global-break-condition))))
      (read-from-minibuffer
       "Global Condition: " initial read-expression-map t
       (if (equal (car read-expression-history) initial)
	   '(read-expression-history . 1)
	 'read-expression-history)))))
  (setq edebug-global-break-condition expression))


;;; Mode switching functions

(defun edebug-set-mode (mode shortmsg msg)
  ;; Set the edebug mode to MODE.
  ;; Display SHORTMSG, or MSG if not within edebug.
  (if (eq (1+ edebug-recursion-depth) (recursion-depth))
      (progn
	(setq edebug-execution-mode mode)
	(message "%s" shortmsg)
	;; Continue execution
	(exit-recursive-edit))
    ;; This is not terribly useful!!
    (setq edebug-next-execution-mode mode)
    (message "%s" msg)))


(defalias 'edebug-step-through-mode 'edebug-step-mode)

(defun edebug-step-mode ()
  "Proceed to next stop point."
  (interactive)
  (edebug-set-mode 'step "" "Edebug will stop at next stop point."))

(defun edebug-next-mode ()
  "Proceed to next `after' stop point."
  (interactive)
  (edebug-set-mode 'next "" "Edebug will stop after next eval."))

(defun edebug-go-mode (arg)
  "Go, evaluating until break.
With prefix ARG, set temporary break at current point and go."
  (interactive "P")
  (if arg
      (edebug-set-breakpoint t))
  (edebug-set-mode 'go "Go..." "Edebug will go until break."))

(defun edebug-Go-nonstop-mode ()
  "Go, evaluating without debugging.
You can use `edebug-stop', or any editing command, to stop."
  (interactive)
  (edebug-set-mode 'Go-nonstop "Go-Nonstop..."
		   "Edebug will not stop at breaks."))


(defun edebug-trace-mode ()
  "Begin trace mode.
Pauses for `edebug-sit-for-seconds' at each stop point."
  (interactive)
  (edebug-set-mode 'trace "Tracing..." "Edebug will trace with pause."))

(defun edebug-Trace-fast-mode ()
  "Trace with no wait at each step.
Updates the display at each stop point, but does not pause."
  (interactive)
  (edebug-set-mode 'Trace-fast
		   "Trace fast..." "Edebug will trace without pause."))

(defun edebug-continue-mode ()
  "Begin continue mode.
Pauses for `edebug-sit-for-seconds' at each break point."
  (interactive)
  (edebug-set-mode 'continue "Continue..."
		   "Edebug will pause at breakpoints."))

(defun edebug-Continue-fast-mode ()
  "Trace with no wait at each step.
Updates the display at each break point, but does not pause."
  (interactive)
  (edebug-set-mode 'Continue-fast "Continue fast..."
		   "Edebug will stop and go at breakpoints."))

;; ------------------------------------------------------------
;; The following use the mode changing commands and breakpoints.


(defun edebug-goto-here ()
  "Proceed to first stop-point at or after current position of point."
  (interactive)
  (edebug-go-mode t))


(defun edebug-stop ()
  "Stop execution and do not continue.
Useful for exiting from trace or continue loop."
  (interactive)
  (message "Stop"))


'(defun edebug-forward ()
  "Proceed to the exit of the next expression to be evaluated."
  (interactive)
  (edebug-set-mode
   'forward "Forward"
   "Edebug will stop after exiting the next expression."))


(defun edebug-forward-sexp (arg)
  "Proceed from the current point to the end of the ARGth sexp ahead.
If there are not ARG sexps ahead, then do `edebug-step-out'."
  (interactive "p")
  (condition-case nil
      (let ((parse-sexp-ignore-comments t))
	;; Call forward-sexp repeatedly until done or failure.
	(forward-sexp arg)
	(edebug-go-mode t))
    (error
     (edebug-step-out)
     )))

(defun edebug-step-out ()
  "Proceed from the current point to the end of the containing sexp.
If there is no containing sexp that is not the top level defun,
go to the end of the last sexp, or if that is the same point, then step."
  (interactive)
  (condition-case nil
      (let ((parse-sexp-ignore-comments t))
	(up-list 1)
	(save-excursion
	  ;; Is there still a containing expression?
	  (up-list 1))
	(edebug-go-mode t))
    (error
     ;; At top level - 1, so first check if there are more sexps at this level.
     (let ((start-point (point)))
;;       (up-list 1)
       (down-list -1)
       (if (= (point) start-point)
	   (edebug-step-mode)	; No more at this level, so step.
	 (edebug-go-mode t)
	 )))))

(defun edebug-instrument-function (func)
  ;; Func should be a function symbol.
  ;; Return the function symbol, or nil if not instrumented.
  (let ((func-marker (get func 'edebug)))
    (cond
     ((and (markerp func-marker) (marker-buffer func-marker))
      ;; It is uninstrumented, so instrument it.
      (with-current-buffer (marker-buffer func-marker)
	(goto-char func-marker)
	(edebug-eval-top-level-form)
	func))
     ((consp func-marker)
      (message "%s is already instrumented." func)
      func)
     (t
      (let ((loc (find-function-noselect func t)))
	(unless (cdr loc)
	  (error "Could not find the definition in its file"))
	(with-current-buffer (car loc)
	  (goto-char (cdr loc))
	  (edebug-eval-top-level-form)
	  func))))))

(defun edebug-instrument-callee ()
  "Instrument the definition of the function or macro about to be called.
Do this when stopped before the form or it will be too late.
One side effect of using this command is that the next time the
function or macro is called, Edebug will be called there as well."
  (interactive)
  (if (not (looking-at "\("))
      (error "You must be before a list form")
    (let ((func
	   (save-excursion
	     (down-list 1)
	     (if (looking-at "\(")
		 (edebug-form-data-name
		  (edebug-get-form-data-entry (point)))
	       (edebug-original-read (current-buffer))))))
      (edebug-instrument-function func))))


(defun edebug-step-in ()
  "Step into the definition of the function or macro about to be called.
This first does `edebug-instrument-callee' to ensure that it is
instrumented.  Then it does `edebug-on-entry' and switches to `go' mode."
  (interactive)
  (let ((func (edebug-instrument-callee)))
    (if func
	(progn
	  (edebug-on-entry func 'temp)
	  (edebug-go-mode nil)))))

(defun edebug-on-entry (function &optional flag)
  "Cause Edebug to stop when FUNCTION is called.
With prefix argument, make this temporary so it is automatically
canceled the first time the function is entered."
  (interactive "aEdebug on entry to: \nP")
  ;; Could store this in the edebug data instead.
  (put function 'edebug-on-entry (if flag 'temp t)))

(defun cancel-edebug-on-entry (function)
  (interactive "aEdebug on entry to: ")
  (put function 'edebug-on-entry nil))


(if (not (fboundp 'edebug-original-debug-on-entry))
    (fset 'edebug-original-debug-on-entry (symbol-function 'debug-on-entry)))
'(fset 'debug-on-entry 'edebug-debug-on-entry)  ;; Should we do this?
;; Also need edebug-cancel-debug-on-entry

'(defun edebug-debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.
If the user continues, FUNCTION's execution proceeds.
Works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use `cancel-debug-on-entry' to cancel the effect of this command.
Redefining FUNCTION also does that.

This version is from Edebug.  If the function is instrumented for
Edebug, it calls `edebug-on-entry'."
  (interactive "aDebug on entry (to function): ")
  (let ((func-data (get function 'edebug)))
    (if (or (null func-data) (markerp func-data))
	(edebug-original-debug-on-entry function)
      (edebug-on-entry function))))


(defun edebug-top-level-nonstop ()
  "Set mode to Go-nonstop, and exit to top-level.
This is useful for exiting even if `unwind-protect' code may be executed."
  (interactive)
  (setq edebug-execution-mode 'Go-nonstop)
  (top-level))


;;(defun edebug-exit-out ()
;;  "Go until the current function exits."
;;  (interactive)
;;  (edebug-set-mode 'exiting "Exit..."))


;;; The following initial mode setting definitions are not used yet.

'(defconst edebug-initial-mode-alist
  '((edebug-Continue-fast . Continue-fast)
    (edebug-Trace-fast . Trace-fast)
    (edebug-continue . continue)
    (edebug-trace . trace)
    (edebug-go . go)
    (edebug-step-through . step)
    (edebug-Go-nonstop . Go-nonstop)
    )
  "Association list between commands and the modes they set.")


'(defun edebug-set-initial-mode ()
  "Ask for the initial mode of the enclosing function.
The mode is requested via the key that would be used to set the mode in
edebug-mode."
  (interactive)
  (let* ((this-function (edebug-which-function))
	 (keymap (if (eq edebug-mode-map (current-local-map))
		     edebug-mode-map))
	 (old-mode (or (get this-function 'edebug-initial-mode)
		       edebug-initial-mode))
	 (key (read-key-sequence
	       (format
		"Change initial edebug mode for %s from %s (%s) to (enter key): "
		       this-function
		       old-mode
		       (where-is-internal
			(car (rassq old-mode edebug-initial-mode-alist))
			keymap 'firstonly
			))))
	 (mode (cdr (assq (key-binding key) edebug-initial-mode-alist)))
	 )
    (if (and mode
	     (or (get this-function 'edebug-initial-mode)
		 (not (eq mode edebug-initial-mode))))
	(progn
	  (put this-function 'edebug-initial-mode mode)
	  (message "Initial mode for %s is now: %s"
		   this-function mode))
      (error "Key must map to one of the mode changing commands")
      )))

;;; Evaluation of expressions

(def-edebug-spec edebug-outside-excursion t)

(defmacro edebug-outside-excursion (&rest body)
  "Evaluate an expression list in the outside context.
Return the result of the last expression."
  `(save-excursion			; of current-buffer
     (if edebug-save-windows
	 (progn
	   ;; After excursion, we will
	   ;; restore to current window configuration.
	   (setq edebug-inside-windows
		 (edebug-current-windows edebug-save-windows))
	   ;; Restore outside windows.
	   (edebug-set-windows edebug-outside-windows)))

     (set-buffer edebug-buffer)		; why?
     ;; (use-local-map edebug-outside-map)
     (set-match-data edebug-outside-match-data)
     ;; Restore outside context.
     (let (;; (edebug-inside-map (current-local-map)) ;; restore map??
	   (last-command-event edebug-outside-last-command-event)
	   (last-command edebug-outside-last-command)
	   (this-command edebug-outside-this-command)
	   (unread-command-char edebug-outside-unread-command-char)
	   (unread-command-events edebug-outside-unread-command-events)
	   (current-prefix-arg edebug-outside-current-prefix-arg)
	   (last-input-event edebug-outside-last-input-event)
	   (last-event-frame edebug-outside-last-event-frame)
	   (last-nonmenu-event edebug-outside-last-nonmenu-event)
	   (track-mouse edebug-outside-track-mouse)
	   (standard-output edebug-outside-standard-output)
	   (standard-input edebug-outside-standard-input)

	   (executing-kbd-macro edebug-outside-executing-macro)
	   (defining-kbd-macro edebug-outside-defining-kbd-macro)
	   ;; Get the values out of the saved statuses.
	   (pre-command-hook (cdr edebug-outside-pre-command-hook))
	   (post-command-hook (cdr edebug-outside-post-command-hook))

	   ;; See edebug-display
	   (overlay-arrow-position edebug-outside-o-a-p)
	   (overlay-arrow-string edebug-outside-o-a-s)
	   (cursor-in-echo-area edebug-outside-c-i-e-a)
	   )
       (setq-default cursor-in-non-selected-windows edebug-outside-d-c-i-n-s-w)
       (unwind-protect
	   (with-current-buffer edebug-outside-buffer ; of edebug-buffer
	     (goto-char edebug-outside-point)
	     (if (marker-buffer (edebug-mark-marker))
		 (set-marker (edebug-mark-marker) edebug-outside-mark))
	     ,@body)

	 ;; Back to edebug-buffer.  Restore rest of inside context.
	 ;; (use-local-map edebug-inside-map)
	 (if edebug-save-windows
	     ;; Restore inside windows.
	     (edebug-set-windows edebug-inside-windows))

	 ;; Save values that may have been changed.
	 (setq
	  edebug-outside-last-command-event last-command-event
	  edebug-outside-last-command last-command
	  edebug-outside-this-command this-command
	  edebug-outside-unread-command-char unread-command-char
	  edebug-outside-unread-command-events unread-command-events
	  edebug-outside-current-prefix-arg current-prefix-arg
	  edebug-outside-last-input-event last-input-event
	  edebug-outside-last-event-frame last-event-frame
	  edebug-outside-last-nonmenu-event last-nonmenu-event
	  edebug-outside-track-mouse track-mouse
	  edebug-outside-standard-output standard-output
	  edebug-outside-standard-input standard-input

	  edebug-outside-executing-macro executing-kbd-macro
	  edebug-outside-defining-kbd-macro defining-kbd-macro

	  edebug-outside-o-a-p overlay-arrow-position
	  edebug-outside-o-a-s overlay-arrow-string
	  edebug-outside-c-i-e-a cursor-in-echo-area
	  edebug-outside-d-c-i-n-s-w (default-value
                                       'cursor-in-non-selected-windows)
          )

	 ;; Restore the outside saved values; don't alter
	 ;; the outside binding loci.
	 (setcdr edebug-outside-pre-command-hook pre-command-hook)
	 (setcdr edebug-outside-post-command-hook post-command-hook)

         (setq-default cursor-in-non-selected-windows t)
	 ))				; let
     ))

(defvar cl-debug-env)  ; defined in cl; non-nil when lexical env used.

(defun edebug-eval (edebug-expr)
  ;; Are there cl lexical variables active?
  (eval (if (bound-and-true-p cl-debug-env)
            (cl-macroexpand-all edebug-expr cl-debug-env)
          edebug-expr)
        lexical-binding))

(defun edebug-safe-eval (edebug-expr)
  ;; Evaluate EXPR safely.
  ;; If there is an error, a string is returned describing the error.
  (condition-case edebug-err
      (edebug-eval edebug-expr)
    (error (edebug-format "%s: %s"  ;; could
			  (get (car edebug-err) 'error-message)
			  (car (cdr edebug-err))))))

;;; Printing


(defun edebug-report-error (edebug-value)
  ;; Print an error message like command level does.
  ;; This also prints the error name if it has no error-message.
  (message "%s: %s"
	   (or (get (car edebug-value) 'error-message)
	       (format "peculiar error (%s)" (car edebug-value)))
	   (mapconcat (function (lambda (edebug-arg)
				  ;; continuing after an error may
				  ;; complain about edebug-arg. why??
				  (prin1-to-string edebug-arg)))
		      (cdr edebug-value) ", ")))

(defvar print-readably) ; defined by lemacs
;; Alternatively, we could change the definition of
;; edebug-safe-prin1-to-string to only use these if defined.

(defun edebug-safe-prin1-to-string (value)
  (let ((print-escape-newlines t)
	(print-length (or edebug-print-length print-length))
	(print-level (or edebug-print-level print-level))
	(print-circle (or edebug-print-circle print-circle))
	(print-readably nil)) ; lemacs uses this.
    (condition-case nil
	(edebug-prin1-to-string value)
      (error "#Apparently circular structure#"))))

(defun edebug-compute-previous-result (edebug-previous-value)
  (if edebug-unwrap-results
      (setq edebug-previous-value
	    (edebug-unwrap* edebug-previous-value)))
  (setq edebug-previous-result
	(concat "Result: "
		(edebug-safe-prin1-to-string edebug-previous-value)
		(eval-expression-print-format edebug-previous-value))))

(defun edebug-previous-result ()
  "Print the previous result."
  (interactive)
  (message "%s" edebug-previous-result))

;;; Read, Eval and Print

(defalias 'edebug-prin1 'prin1)
(defalias 'edebug-print 'print)
(defalias 'edebug-prin1-to-string 'prin1-to-string)
(defalias 'edebug-format 'format)
(defalias 'edebug-message 'message)

(defun edebug-eval-expression (edebug-expr)
  "Evaluate an expression in the outside environment.
If interactive, prompt for the expression.
Print result in minibuffer."
  (interactive (list (read-from-minibuffer
		      "Eval: " nil read-expression-map t
		      'read-expression-history)))
  (princ
   (edebug-outside-excursion
    (setq values (cons (edebug-eval edebug-expr) values))
    (concat (edebug-safe-prin1-to-string (car values))
            (eval-expression-print-format (car values))))))

(defun edebug-eval-last-sexp ()
  "Evaluate sexp before point in the outside environment.
Print value in minibuffer."
  (interactive)
  (edebug-eval-expression (edebug-last-sexp)))

(defun edebug-eval-print-last-sexp ()
  "Evaluate sexp before point in outside environment; insert value.
This prints the value into current buffer."
  (interactive)
  (let* ((edebug-form (edebug-last-sexp))
	 (edebug-result-string
	  (edebug-outside-excursion
	   (edebug-safe-prin1-to-string (edebug-safe-eval edebug-form))))
	 (standard-output (current-buffer)))
    (princ "\n")
    ;; princ the string to get rid of quotes.
    (princ edebug-result-string)
    (princ "\n")
    ))

;;; Edebug Minor Mode

;; FIXME eh?
(defvar gud-inhibit-global-bindings
  "*Non-nil means don't do global rebindings of C-x C-a subcommands.")

;; Global GUD bindings for all emacs-lisp-mode buffers.
(unless gud-inhibit-global-bindings
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-s" 'edebug-step-mode)
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-n" 'edebug-next-mode)
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-c" 'edebug-go-mode)
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-l" 'edebug-where))

(defvar edebug-mode-map
  (let ((map (copy-keymap emacs-lisp-mode-map)))
    ;; control
    (define-key map " " 'edebug-step-mode)
    (define-key map "n" 'edebug-next-mode)
    (define-key map "g" 'edebug-go-mode)
    (define-key map "G" 'edebug-Go-nonstop-mode)
    (define-key map "t" 'edebug-trace-mode)
    (define-key map "T" 'edebug-Trace-fast-mode)
    (define-key map "c" 'edebug-continue-mode)
    (define-key map "C" 'edebug-Continue-fast-mode)

    ;;(define-key map "f" 'edebug-forward) not implemented
    (define-key map "f" 'edebug-forward-sexp)
    (define-key map "h" 'edebug-goto-here)

    (define-key map "I" 'edebug-instrument-callee)
    (define-key map "i" 'edebug-step-in)
    (define-key map "o" 'edebug-step-out)

    ;; quitting and stopping
    (define-key map "q" 'top-level)
    (define-key map "Q" 'edebug-top-level-nonstop)
    (define-key map "a" 'abort-recursive-edit)
    (define-key map "S" 'edebug-stop)

    ;; breakpoints
    (define-key map "b" 'edebug-set-breakpoint)
    (define-key map "u" 'edebug-unset-breakpoint)
    (define-key map "B" 'edebug-next-breakpoint)
    (define-key map "x" 'edebug-set-conditional-breakpoint)
    (define-key map "X" 'edebug-set-global-break-condition)

    ;; evaluation
    (define-key map "r" 'edebug-previous-result)
    (define-key map "e" 'edebug-eval-expression)
    (define-key map "\C-x\C-e" 'edebug-eval-last-sexp)
    (define-key map "E" 'edebug-visit-eval-list)

    ;; views
    (define-key map "w" 'edebug-where)
    (define-key map "v" 'edebug-view-outside) ;; maybe obsolete??
    (define-key map "p" 'edebug-bounce-point)
    (define-key map "P" 'edebug-view-outside) ;; same as v
    (define-key map "W" 'edebug-toggle-save-windows)

    ;; misc
    (define-key map "?" 'edebug-help)
    (define-key map "d" 'edebug-backtrace)

    (define-key map "-" 'negative-argument)

    ;; statistics
    (define-key map "=" 'edebug-temp-display-freq-count)

    ;; GUD bindings
    (define-key map "\C-c\C-s" 'edebug-step-mode)
    (define-key map "\C-c\C-n" 'edebug-next-mode)
    (define-key map "\C-c\C-c" 'edebug-go-mode)

    (define-key map "\C-x " 'edebug-set-breakpoint)
    (define-key map "\C-c\C-d" 'edebug-unset-breakpoint)
    (define-key map "\C-c\C-t"
      (lambda () (interactive) (edebug-set-breakpoint t)))
    (define-key map "\C-c\C-l" 'edebug-where)
    map))

;; Autoloading these global bindings doesn't make sense because
;; they cannot be used anyway unless Edebug is already loaded and active.

(defvar global-edebug-prefix "\^XX"
  "Prefix key for global edebug commands, available from any buffer.")

(defvar global-edebug-map
  (let ((map (make-sparse-keymap)))

    (define-key map " " 'edebug-step-mode)
    (define-key map "g" 'edebug-go-mode)
    (define-key map "G" 'edebug-Go-nonstop-mode)
    (define-key map "t" 'edebug-trace-mode)
    (define-key map "T" 'edebug-Trace-fast-mode)
    (define-key map "c" 'edebug-continue-mode)
    (define-key map "C" 'edebug-Continue-fast-mode)

    ;; breakpoints
    (define-key map "b" 'edebug-set-breakpoint)
    (define-key map "u" 'edebug-unset-breakpoint)
    (define-key map "x" 'edebug-set-conditional-breakpoint)
    (define-key map "X" 'edebug-set-global-break-condition)

    ;; views
    (define-key map "w" 'edebug-where)
    (define-key map "W" 'edebug-toggle-save-windows)

    ;; quitting
    (define-key map "q" 'top-level)
    (define-key map "Q" 'edebug-top-level-nonstop)
    (define-key map "a" 'abort-recursive-edit)

    ;; statistics
    (define-key map "=" 'edebug-display-freq-count)
    map)
  "Global map of edebug commands, available from any buffer.")

(global-unset-key global-edebug-prefix)
(global-set-key global-edebug-prefix global-edebug-map)


(defun edebug-help ()
  "Describe `edebug-mode'."
  (interactive)
  (describe-function 'edebug-mode))

(defun edebug-mode ()
  "Mode for Emacs Lisp buffers while in Edebug.

In addition to all Emacs Lisp commands (except those that modify the
buffer) there are local and global key bindings to several Edebug
specific commands.  E.g. `edebug-step-mode' is bound to \\[edebug-step-mode]
in the Edebug buffer and \\<global-map>\\[edebug-step-mode] in any buffer.

Also see bindings for the eval list buffer *edebug* in `edebug-eval-mode'.

The edebug buffer commands:
\\{edebug-mode-map}

Global commands prefixed by `global-edebug-prefix':
\\{global-edebug-map}

Options:
`edebug-setup-hook'
`edebug-all-defs'
`edebug-all-forms'
`edebug-save-windows'
`edebug-save-displayed-buffer-points'
`edebug-initial-mode'
`edebug-trace'
`edebug-test-coverage'
`edebug-continue-kbd-macro'
`edebug-print-length'
`edebug-print-level'
`edebug-print-circle'
`edebug-on-error'
`edebug-on-quit'
`edebug-on-signal'
`edebug-unwrap-results'
`edebug-global-break-condition'"
  ;; If the user kills the buffer in which edebug is currently active,
  ;; exit to top level, because the edebug command loop can't usefully
  ;; continue running in such a case.
  (add-hook 'kill-buffer-hook 'edebug-kill-buffer nil t)
  (use-local-map edebug-mode-map))

(defun edebug-kill-buffer ()
  "Used on `kill-buffer-hook' when Edebug is operating in a buffer of Lisp code."
  (let (kill-buffer-hook)
    (kill-buffer (current-buffer)))
  (top-level))

;;; edebug eval list mode

;; A list of expressions and their evaluations is displayed in *edebug*.

(defun edebug-eval-result-list ()
  "Return a list of evaluations of `edebug-eval-list'."
  ;; Assumes in outside environment.
  ;; Don't do any edebug things now.
  (let ((edebug-execution-mode 'Go-nonstop)
	(edebug-trace nil))
    (mapcar 'edebug-safe-eval edebug-eval-list)))

(defun edebug-eval-display-list (edebug-eval-result-list)
  ;; Assumes edebug-eval-buffer exists.
  (let ((edebug-eval-list-temp edebug-eval-list)
	(standard-output edebug-eval-buffer)
	(edebug-comment-line
	 (format ";%s\n" (make-string (- (window-width) 2) ?-))))
    (set-buffer edebug-eval-buffer)
    (erase-buffer)
    (while edebug-eval-list-temp
      (prin1 (car edebug-eval-list-temp)) (terpri)
      (prin1 (car edebug-eval-result-list)) (terpri)
      (princ edebug-comment-line)
      (setq edebug-eval-list-temp (cdr edebug-eval-list-temp))
      (setq edebug-eval-result-list (cdr edebug-eval-result-list)))
    (edebug-pop-to-buffer edebug-eval-buffer)
    ))

(defun edebug-create-eval-buffer ()
  (if (not (and edebug-eval-buffer (buffer-name edebug-eval-buffer)))
      (progn
	(set-buffer (setq edebug-eval-buffer (get-buffer-create "*edebug*")))
	(edebug-eval-mode))))

;; Should generalize this to be callable outside of edebug
;; with calls in user functions, e.g. (edebug-eval-display)

(defun edebug-eval-display (edebug-eval-result-list)
  "Display expressions and evaluations in EDEBUG-EVAL-RESULT-LIST.
It modifies the context by popping up the eval display."
  (if edebug-eval-result-list
      (progn
	(edebug-create-eval-buffer)
	(edebug-eval-display-list edebug-eval-result-list)
	)))

(defun edebug-eval-redisplay ()
  "Redisplay eval list in outside environment.
May only be called from within `edebug-recursive-edit'."
  (edebug-create-eval-buffer)
  (edebug-outside-excursion
   (edebug-eval-display-list (edebug-eval-result-list))
   ))

(defun edebug-visit-eval-list ()
  "Switch to the evaluation list buffer \"*edebug*\"."
  (interactive)
  (edebug-eval-redisplay)
  (edebug-pop-to-buffer edebug-eval-buffer))


(defun edebug-update-eval-list ()
  "Replace the evaluation list with the sexps now in the eval buffer."
  (interactive)
  (let ((starting-point (point))
	new-list)
    (goto-char (point-min))
    ;; get the first expression
    (edebug-skip-whitespace)
    (if (not (eobp))
	(progn
	  (forward-sexp 1)
	  (setq new-list (cons (edebug-last-sexp) new-list))))

    (while (re-search-forward "^;" nil t)
      (forward-line 1)
      (skip-chars-forward " \t\n\r")
      (if (and (/= ?\; (following-char))
	       (not (eobp)))
	  (progn
	    (forward-sexp 1)
	    (setq new-list (cons (edebug-last-sexp) new-list)))))

    (setq edebug-eval-list (nreverse new-list))
    (edebug-eval-redisplay)
    (goto-char starting-point)))


(defun edebug-delete-eval-item ()
  "Delete the item under point and redisplay."
  ;; could add arg to do repeatedly
  (interactive)
  (if (re-search-backward "^;" nil 'nofail)
      (forward-line 1))
  (delete-region
   (point) (progn (re-search-forward "^;" nil 'nofail)
		  (beginning-of-line)
		  (point)))
  (edebug-update-eval-list))



(defvar edebug-eval-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-interaction-mode-map)
    (define-key map "\C-c\C-w" 'edebug-where)
    (define-key map "\C-c\C-d" 'edebug-delete-eval-item)
    (define-key map "\C-c\C-u" 'edebug-update-eval-list)
    (define-key map "\C-x\C-e" 'edebug-eval-last-sexp)
    (define-key map "\C-j" 'edebug-eval-print-last-sexp)
  map)
"Keymap for Edebug Eval mode.  Superset of Lisp Interaction mode.")

(put 'edebug-eval-mode 'mode-class 'special)

(define-derived-mode edebug-eval-mode lisp-interaction-mode "Edebug Eval"
  "Mode for evaluation list buffer while in Edebug.

In addition to all Interactive Emacs Lisp commands there are local and
global key bindings to several Edebug specific commands.  E.g.
`edebug-step-mode' is bound to \\[edebug-step-mode] in the Edebug
buffer and \\<global-map>\\[edebug-step-mode] in any buffer.

Eval list buffer commands:
\\{edebug-eval-mode-map}

Global commands prefixed by `global-edebug-prefix':
\\{global-edebug-map}")

;;; Interface with standard debugger.

;; (setq debugger 'edebug) ; to use the edebug debugger
;; (setq debugger 'debug)  ; use the standard debugger

;; Note that debug and its utilities must be byte-compiled to work,
;; since they depend on the backtrace looking a certain way.  But
;; edebug is not dependent on this, yet.

(defun edebug (&optional edebug-arg-mode &rest debugger-args)
  "Replacement for `debug'.
If we are running an edebugged function, show where we last were.
Otherwise call `debug' normally."
;;  (message "entered: %s  depth: %s  edebug-recursion-depth: %s"
;;	   edebug-entered (recursion-depth) edebug-recursion-depth) (sit-for 1)
  (if (and edebug-entered  ; anything active?
	   (eq (recursion-depth) edebug-recursion-depth))
      (let (;; Where were we before the error occurred?
	    (edebug-offset-index (car edebug-offset-indices))
	    ;; Bind variables required by edebug-display
	    (edebug-value (car debugger-args))
	    edebug-breakpoints
	    edebug-break-data
	    edebug-break-condition
	    edebug-global-break
	    (edebug-break (null edebug-arg-mode)) ;; if called explicitly
	    )
	(edebug-display)
	(if (eq edebug-arg-mode 'error)
	    nil
	  edebug-value))

    ;; Otherwise call debug normally.
    ;; Still need to remove extraneous edebug calls from stack.
    (apply 'debug edebug-arg-mode debugger-args)
    ))


(defun edebug-backtrace ()
  "Display a non-working backtrace.  Better than nothing..."
  (interactive)
  (if (or (not edebug-backtrace-buffer)
	  (null (buffer-name edebug-backtrace-buffer)))
      (setq edebug-backtrace-buffer
	    (generate-new-buffer "*Backtrace*"))
    ;; else, could just display edebug-backtrace-buffer
    )
  (with-output-to-temp-buffer (buffer-name edebug-backtrace-buffer)
    (setq edebug-backtrace-buffer standard-output)
    (let ((print-escape-newlines t)
	  (print-length 50)	; FIXME cf edebug-safe-prin1-to-string
	  last-ok-point)
      (backtrace)

      ;; Clean up the backtrace.
      ;; Not quite right for current edebug scheme.
      (set-buffer edebug-backtrace-buffer)
      (setq truncate-lines t)
      (goto-char (point-min))
      (setq last-ok-point (point))
      (if t (progn

      ;; Delete interspersed edebug internals.
      (while (re-search-forward "^  \(?edebug" nil t)
	(beginning-of-line)
	(cond
	 ((looking-at "^  \(edebug-after")
	  ;; Previous lines may contain code, so just delete this line
	  (setq last-ok-point (point))
	  (forward-line 1)
	  (delete-region last-ok-point (point)))

	 ((looking-at "^  edebug")
	  (forward-line 1)
	  (delete-region last-ok-point (point))
	  )))
      )))))


;;; Trace display

(defun edebug-trace-display (buf-name fmt &rest args)
  "In buffer BUF-NAME, display FMT and ARGS at the end and make it visible.
The buffer is created if it does not exist.
You must include newlines in FMT to break lines, but one newline is appended."
;; e.g.
;;	 (edebug-trace-display "*trace-point*"
;;	  "saving: point = %s  window-start = %s"
;;	  (point) (window-start))
  (let* ((oldbuf (current-buffer))
	 (selected-window (selected-window))
	 (buffer (get-buffer-create buf-name))
	 buf-window)
;;    (message "before pop-to-buffer") (sit-for 1)
    (edebug-pop-to-buffer buffer)
    (setq truncate-lines t)
    (setq buf-window (selected-window))
    (goto-char (point-max))
    (insert (apply 'edebug-format fmt args) "\n")
    ;; Make it visible.
    (vertical-motion (- 1 (window-height)))
    (set-window-start buf-window (point))
    (goto-char (point-max))
;;    (set-window-point buf-window (point))
;;    (edebug-sit-for 0)
    (bury-buffer buffer)
    (select-window selected-window)
    (set-buffer oldbuf))
  buf-name)


(defun edebug-trace (fmt &rest args)
  "Convenience call to `edebug-trace-display' using `edebug-trace-buffer'."
  (apply 'edebug-trace-display edebug-trace-buffer fmt args))


;;; Frequency count and coverage

;; FIXME should this use overlays instead?
;; Definitely, IMO.  The current business with undo in
;; edebug-temp-display-freq-count is horrid.
(defun edebug-display-freq-count ()
  "Display the frequency count data for each line of the current definition.
The frequency counts are inserted as comment lines after each line,
and you can undo all insertions with one `undo' command.

The counts are inserted starting under the `(' before an expression
or the `)' after an expression, or on the last char of a symbol.
The counts are only displayed when they differ from previous counts on
the same line.

If coverage is being tested, whenever all known results of an expression
are `eq', the char `=' will be appended after the count
for that expression.  Note that this is always the case for an
expression only evaluated once.

To clear the frequency count and coverage data for a definition,
reinstrument it."
  (interactive)
  (let* ((function (edebug-form-data-symbol))
	 (counts (get function 'edebug-freq-count))
	 (coverages (get function 'edebug-coverage))
	 (data (get function 'edebug))
	 (def-mark (car data))	; mark at def start
	 (edebug-points (nth 2 data))
	 (i (1- (length edebug-points)))
	 (last-index)
	 (first-index)
	 (start-of-line)
	 (start-of-count-line)
	 (last-count)
	 )
    (save-excursion
      ;; Traverse in reverse order so offsets are correct.
      (while (<= 0 i)
	;; Start at last expression in line.
	(goto-char (+ def-mark (aref edebug-points i)))
	(beginning-of-line)
	(setq start-of-line (- (point) def-mark)
	      last-index i)

	;; Find all indexes on same line.
	(while (and (<= 0 (setq i (1- i)))
		    (<= start-of-line (aref edebug-points i))))
	;; Insert all the indices for this line.
	(forward-line 1)
	(setq start-of-count-line (point)
	      first-index i   ; really last index for line above this one.
	      last-count -1)  ; cause first count to always appear.
	(insert ";#")
	;; i == first-index still
	(while (<= (setq i (1+ i)) last-index)
	  (let ((count (aref counts i))
		(coverage (aref coverages i))
		(col (save-excursion
		       (goto-char (+ (aref edebug-points i) def-mark))
		       (- (current-column)
			  (if (= ?\( (following-char)) 0 1)))))
	    (insert (make-string
		     (max 0 (- col (- (point) start-of-count-line))) ?\s)
		    (if (and (< 0 count)
			     (not (memq coverage
					'(unknown ok-coverage))))
			"=" "")
		    (if (= count last-count) "" (int-to-string count))
		    " ")
	    (setq last-count count)))
	(insert "\n")
	(setq i first-index)))))

;; FIXME this does not work very well.  Eg if you press an arrow key,
;; or make a mouse-click, it fails with "Non-character input-event".
(defun edebug-temp-display-freq-count ()
  "Temporarily display the frequency count data for the current definition.
It is removed when you hit any char."
  ;; This seems not to work with Emacs 18.59. It undoes too far.
  (interactive)
  (let ((buffer-read-only nil))
    (undo-boundary)
    (edebug-display-freq-count)
    (setq unread-command-char (read-char))
    ;; Yuck!  This doesn't seem to work at all for me.
    (undo)))


;;; Menus

(defun edebug-toggle (variable)
  (set variable (not (symbol-value variable)))
  (message "%s: %s" variable (symbol-value variable)))

;; We have to require easymenu (even for Emacs 18) just so
;; the easy-menu-define macro call is compiled correctly.
(require 'easymenu)

(defconst edebug-mode-menus
  '("Edebug"
     ["Stop" edebug-stop t]
     ["Step" edebug-step-mode t]
     ["Next" edebug-next-mode t]
     ["Trace" edebug-trace-mode t]
     ["Trace Fast" edebug-Trace-fast-mode t]
     ["Continue" edebug-continue-mode t]
     ["Continue Fast" edebug-Continue-fast-mode t]
     ["Go" edebug-go-mode t]
     ["Go Nonstop" edebug-Go-nonstop-mode t]
     "----"
     ["Help" edebug-help t]
     ["Abort" abort-recursive-edit t]
     ["Quit to Top Level"  top-level t]
     ["Quit Nonstop" edebug-top-level-nonstop t]
     "----"
    ("Jumps"
     ["Forward Sexp" edebug-forward-sexp t]
     ["Step In" edebug-step-in t]
     ["Step Out" edebug-step-out t]
     ["Goto Here" edebug-goto-here t])

    ("Breaks"
     ["Set Breakpoint" edebug-set-breakpoint t]
     ["Unset Breakpoint" edebug-unset-breakpoint t]
     ["Set Conditional Breakpoint" edebug-set-conditional-breakpoint t]
     ["Set Global Break Condition" edebug-set-global-break-condition t]
     ["Show Next Breakpoint" edebug-next-breakpoint t])

    ("Views"
     ["Where am I?" edebug-where t]
     ["Bounce to Current Point" edebug-bounce-point t]
     ["View Outside Windows" edebug-view-outside t]
     ["Previous Result" edebug-previous-result t]
     ["Show Backtrace" edebug-backtrace t]
     ["Display Freq Count" edebug-display-freq-count t])

    ("Eval"
     ["Expression" edebug-eval-expression t]
     ["Last Sexp" edebug-eval-last-sexp t]
     ["Visit Eval List" edebug-visit-eval-list t])

    ("Options"
     ["Edebug All Defs" edebug-all-defs
      :style toggle :selected edebug-all-defs]
     ["Edebug All Forms" edebug-all-forms
      :style toggle :selected edebug-all-forms]
     "----"
     ["Tracing" (edebug-toggle 'edebug-trace)
      :style toggle :selected edebug-trace]
     ["Test Coverage" (edebug-toggle 'edebug-test-coverage)
      :style toggle :selected edebug-test-coverage]
     ["Save Windows" edebug-toggle-save-windows
      :style toggle :selected edebug-save-windows]
     ["Save Point"
      (edebug-toggle 'edebug-save-displayed-buffer-points)
      :style toggle :selected edebug-save-displayed-buffer-points]
     ))
  "Menus for Edebug.")


;;; Emacs version specific code

(defalias 'edebug-window-live-p 'window-live-p)

(defun edebug-mark ()
  (mark t))

(defun edebug-set-conditional-breakpoint (arg condition)
  "Set a conditional breakpoint at nearest sexp.
The condition is evaluated in the outside context.
With prefix argument, make it a temporary breakpoint."
  ;; (interactive "P\nxCondition: ")
  (interactive
   (list
    current-prefix-arg
    ;; Read condition as follows; getting previous condition is cumbersome:
    (let ((edebug-stop-point (edebug-find-stop-point)))
      (if edebug-stop-point
	  (let* ((edebug-def-name (car edebug-stop-point))
		 (index (cdr edebug-stop-point))
		 (edebug-data (get edebug-def-name 'edebug))
		 (edebug-breakpoints (car (cdr edebug-data)))
		 (edebug-break-data (assq index edebug-breakpoints))
		 (edebug-break-condition (car (cdr edebug-break-data)))
		 (initial (and edebug-break-condition
			       (format "%s" edebug-break-condition))))
	    (read-from-minibuffer
	     "Condition: " initial read-expression-map t
	     (if (equal (car read-expression-history) initial)
		 '(read-expression-history . 1)
	       'read-expression-history)))))))
  (edebug-modify-breakpoint t condition arg))

(easy-menu-define edebug-menu edebug-mode-map "Edebug menus" edebug-mode-menus)

;;; Byte-compiler

;; Extension for bytecomp to resolve undefined function references.
;; Requires new byte compiler.

;; Reenable byte compiler warnings about unread-command-char and -event.
;; Disabled before edebug-recursive-edit.
(eval-when-compile
  (if edebug-unread-command-char-warning
      (put 'unread-command-char 'byte-obsolete-variable
	   edebug-unread-command-char-warning)))

(eval-when-compile
  ;; The body of eval-when-compile seems to get evaluated with eval-defun.
  ;; We only want to evaluate when actually byte compiling.
  ;; But it is OK to evaluate as long as byte-compiler has been loaded.
  (if (featurep 'byte-compile) (progn

  (defun byte-compile-resolve-functions (funcs)
    "Say it is OK for the named functions to be unresolved."
    (mapc
     (function
      (lambda (func)
	(setq byte-compile-unresolved-functions
	      (delq (assq func byte-compile-unresolved-functions)
		    byte-compile-unresolved-functions))))
     funcs)
    nil)

  '(defun byte-compile-resolve-free-references (vars)
     "Say it is OK for the named variables to be referenced."
     (mapcar
      (function
       (lambda (var)
	 (setq byte-compile-free-references
	       (delq var byte-compile-free-references))))
      vars)
     nil)

  '(defun byte-compile-resolve-free-assignments (vars)
     "Say it is OK for the named variables to be assigned."
     (mapcar
      (function
       (lambda (var)
	 (setq byte-compile-free-assignments
	       (delq var byte-compile-free-assignments))))
      vars)
     nil)

  (byte-compile-resolve-functions
   '(reporter-submit-bug-report
     edebug-gensym ;; also in cl.el
     ;; Interfaces to standard functions.
     edebug-original-eval-defun
     edebug-original-read
     edebug-get-buffer-window
     edebug-mark
     edebug-mark-marker
     edebug-input-pending-p
     edebug-sit-for
     edebug-prin1-to-string
     edebug-format
     ;; lemacs
     zmacs-deactivate-region
     popup-menu
     ;; CL
     cl-macroexpand-all
     ;; And believe it or not, the byte compiler doesn't know about:
     byte-compile-resolve-functions
     ))

  '(byte-compile-resolve-free-references
    '(read-expression-history
      read-expression-map))

  '(byte-compile-resolve-free-assignments
    '(read-expression-history))

  )))


;;; Autoloading of Edebug accessories

(if (featurep 'cl)
    (add-hook 'edebug-setup-hook
	      (function (lambda () (require 'cl-specs))))
  ;; The following causes cl-specs to be loaded if you load cl.el.
  (add-hook 'cl-load-hook
	    (function (lambda () (require 'cl-specs)))))

;; edebug-cl-read and cl-read are available from liberte@cs.uiuc.edu
(if (featurep 'cl-read)
    (add-hook 'edebug-setup-hook
	      (function (lambda () (require 'edebug-cl-read))))
  ;; The following causes edebug-cl-read to be loaded when you load cl-read.el.
  (add-hook 'cl-read-load-hooks
	    (function (lambda () (require 'edebug-cl-read)))))


;;; Finalize Loading

;; Finally, hook edebug into the rest of Emacs.
;; There are probably some other things that could go here.

;; Install edebug read and eval functions.
(edebug-install-read-eval-functions)

(provide 'edebug)

;;; edebug.el ends here
