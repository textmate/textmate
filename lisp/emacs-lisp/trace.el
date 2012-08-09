;;; trace.el --- tracing facility for Emacs Lisp functions

;; Copyright (C) 1993, 1998, 2000-2012 Free Software Foundation, Inc.

;; Author: Hans Chalupsky <hans@cs.buffalo.edu>
;; Maintainer: FSF
;; Created: 15 Dec 1992
;; Keywords: tools, lisp

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

;; LCD Archive Entry:
;; trace|Hans Chalupsky|hans@cs.buffalo.edu|
;; Tracing facility for Emacs Lisp functions|
;; 1993/05/18 00:41:16|2.0|~/packages/trace.el.Z|


;;; Commentary:

;; Introduction:
;; =============
;; A simple trace package that utilizes advice.el. It generates trace
;; information in a Lisp-style fashion and inserts it into a trace output
;; buffer. Tracing can be done in the background (or silently) so that
;; generation of trace output won't interfere with what you are currently
;; doing.

;; Requirement:
;; ============
;; trace.el needs advice.el version 2.0 or later which you can get from the
;; same place from where you got trace.el.

;; Restrictions:
;; =============
;; - Traced subrs when called interactively will always show nil as the
;;   value of their arguments.
;; - Only functions/macros/subrs that are called via their function cell will
;;   generate trace output, hence, you won't get trace output for:
;;   + Subrs called directly from other subrs/C-code
;;   + Compiled calls to subrs that have special byte-codes associated
;;     with them (e.g., car, cdr, ...)
;;   + Macros that were expanded during compilation
;; - All the restrictions that apply to advice.el

;; Installation:
;; =============
;; Put this file together with advice.el (version 2.0 or later) somewhere
;; into your Emacs `load-path', byte-compile it/them for efficiency, and
;; put the following autoload declarations into your .emacs
;;
;;    (autoload 'trace-function "trace" "Trace a function" t)
;;    (autoload 'trace-function-background "trace" "Trace a function" t)
;;
;; or explicitly load it with (require 'trace) or (load "trace").

;; Usage:
;; ======
;; - To trace a function say `M-x trace-function' which will ask you for the
;;   name of the function/subr/macro to trace, as well as for the buffer
;;   into which trace output should go.
;; - If you want to trace a function that switches buffers or does other
;;   display oriented stuff use `M-x trace-function-background' which will
;;   generate the trace output silently in the background without popping
;;   up windows and doing other irritating stuff.
;; - To untrace a function say `M-x untrace-function'.
;; - To untrace all currently traced functions say `M-x untrace-all'.

;; Examples:
;; =========
;;
;;  (defun fact (n)
;;    (if (= n 0) 1
;;      (* n (fact (1- n)))))
;;  fact
;;
;;  (trace-function 'fact)
;;  fact
;;
;;  Now, evaluating this...
;;
;;  (fact 4)
;;  24
;;
;;  ...will generate the following in *trace-buffer*:
;;
;;  1 -> fact: n=4
;;  | 2 -> fact: n=3
;;  | | 3 -> fact: n=2
;;  | | | 4 -> fact: n=1
;;  | | | | 5 -> fact: n=0
;;  | | | | 5 <- fact: 1
;;  | | | 4 <- fact: 1
;;  | | 3 <- fact: 2
;;  | 2 <- fact: 6
;;  1 <- fact: 24
;;
;;
;;  (defun ack (x y z)
;;    (if (= x 0)
;;        (+ y z)
;;      (if (and (<= x 2) (= z 0))
;;          (1- x)
;;        (if (and (> x 2) (= z 0))
;;            y
;;          (ack (1- x) y (ack x y (1- z)))))))
;;  ack
;;
;;  (trace-function 'ack)
;;  ack
;;
;;  Try this for some interesting trace output:
;;
;;  (ack 3 3 1)
;;  27
;;
;;
;; The following does something similar to the functionality of the package
;; log-message.el by Robert Potter, which is giving you a chance to look at
;; messages that might have whizzed by too quickly (you won't see subr
;; generated messages though):
;;
;; (trace-function-background 'message "*Message Log*")


;;; Change Log:

;; Revision 2.0 1993/05/18 00:41:16 hans
;;	* Adapted for advice.el 2.0; it now also works
;;	  for GNU Emacs-19 and Lemacs
;;	* Separate function `trace-function-background'
;;	* Separate pieces of advice for foreground and background tracing
;;	* Less insane handling of interactive trace buffer specification
;;	* String arguments and values are now printed properly
;;
;; Revision 1.1 1992/12/15 22:45:15 hans
;;	* Created, first public release


;;; Code:

(require 'advice)

(defgroup trace nil
  "Tracing facility for Emacs Lisp functions."
  :prefix "trace-"
  :group 'lisp)

;;;###autoload
(defcustom trace-buffer (purecopy "*trace-output*")
  "Trace output will by default go to that buffer."
  :type 'string
  :group 'trace)

;; Current level of traced function invocation:
(defvar trace-level 0)

;; Semi-cryptic name used for a piece of trace advice:
(defvar trace-advice-name 'trace-function\ )

;; Used to separate new trace output from previous traced runs:
(defvar trace-separator (format "%s\n" (make-string 70 ?=)))

(defvar inhibit-trace nil
  "If non-nil, all tracing is temporarily inhibited.")

(defun trace-entry-message (function level argument-bindings)
  ;; Generates a string that describes that FUNCTION has been entered at
  ;; trace LEVEL with ARGUMENT-BINDINGS.
  (format "%s%s%d -> %s: %s\n"
	  (mapconcat 'char-to-string (make-string (1- level) ?|) " ")
	  (if (> level 1) " " "")
	  level
	  function
          (let ((print-circle t))
            (mapconcat (lambda (binding)
                         (concat
                          (symbol-name (ad-arg-binding-field binding 'name))
                          "="
                          ;; do this so we'll see strings:
                          (prin1-to-string
                           (ad-arg-binding-field binding 'value))))
                       argument-bindings
                       " "))))

(defun trace-exit-message (function level value)
  ;; Generates a string that describes that FUNCTION has been exited at
  ;; trace LEVEL and that it returned VALUE.
  (format "%s%s%d <- %s: %s\n"
	  (mapconcat 'char-to-string (make-string (1- level) ?|) " ")
	  (if (> level 1) " " "")
	  level
	  function
	  ;; do this so we'll see strings:
	  (let ((print-circle t)) (prin1-to-string value))))

(defun trace-make-advice (function buffer background)
  ;; Builds the piece of advice to be added to FUNCTION's advice info
  ;; so that it will generate the proper trace output in BUFFER
  ;; (quietly if BACKGROUND is t).
  (ad-make-advice
   trace-advice-name nil t
   `(advice
     lambda ()
     (let ((trace-level (1+ trace-level))
	   (trace-buffer (get-buffer-create ,buffer)))
       (unless inhibit-trace
	 (with-current-buffer trace-buffer
	   (set (make-local-variable 'window-point-insertion-type) t)
           ,(unless background '(display-buffer trace-buffer))
	   (goto-char (point-max))
	   ;; Insert a separator from previous trace output:
	   (if (= trace-level 1) (insert trace-separator))
	   (insert
	    (trace-entry-message
	     ',function trace-level ad-arg-bindings))))
       ad-do-it
       (unless inhibit-trace
	 (with-current-buffer trace-buffer
	   ,(unless background '(display-buffer trace-buffer))
	   (goto-char (point-max))
	   (insert
	    (trace-exit-message
	     ',function trace-level ad-return-value))))))))

(defun trace-function-internal (function buffer background)
  ;; Adds trace advice for FUNCTION and activates it.
  (ad-add-advice
   function
   (trace-make-advice function (or buffer trace-buffer) background)
   'around 'last)
  (ad-activate function nil))

(defun trace-is-traced (function)
  (ad-find-advice function 'around trace-advice-name))

;;;###autoload
(defun trace-function (function &optional buffer)
  "Traces FUNCTION with trace output going to BUFFER.
For every call of FUNCTION Lisp-style trace messages that display argument
and return values will be inserted into BUFFER.  This function generates the
trace advice for FUNCTION and activates it together with any other advice
there might be!!  The trace BUFFER will popup whenever FUNCTION is called.
Do not use this to trace functions that switch buffers or do any other
display oriented stuff, use `trace-function-background' instead."
  (interactive
   (list
    (intern (completing-read "Trace function: " obarray 'fboundp t))
    (read-buffer "Output to buffer: " trace-buffer)))
  (trace-function-internal function buffer nil))

;;;###autoload
(defun trace-function-background (function &optional buffer)
  "Traces FUNCTION with trace output going quietly to BUFFER.
When this tracing is enabled, every call to FUNCTION writes
a Lisp-style trace message (showing the arguments and return value)
into BUFFER.  This function generates advice to trace FUNCTION
and activates it together with any other advice there might be.
The trace output goes to BUFFER quietly, without changing
the window or buffer configuration.

BUFFER defaults to `trace-buffer'."
  (interactive
   (list
    (intern
     (completing-read "Trace function in background: " obarray 'fboundp t))
    (read-buffer "Output to buffer: " trace-buffer)))
  (trace-function-internal function buffer t))

(defun untrace-function (function)
  "Untraces FUNCTION and possibly activates all remaining advice.
Activation is performed with `ad-update', hence remaining advice will get
activated only if the advice of FUNCTION is currently active.  If FUNCTION
was not traced this is a noop."
  (interactive
   (list (ad-read-advised-function "Untrace function: " 'trace-is-traced)))
  (when (trace-is-traced function)
    (ad-remove-advice function 'around trace-advice-name)
    (ad-update function)))

(defun untrace-all ()
  "Untraces all currently traced functions."
  (interactive)
  (ad-do-advised-functions (function)
    (untrace-function function)))

(provide 'trace)

;;; trace.el ends here
