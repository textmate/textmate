;;; benchmark.el --- support for benchmarking code

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author: Dave Love  <fx@gnu.org>
;; Keywords: lisp, extensions

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

;; Utilities for timing the execution of forms, including the time
;; taken for GC.  Note that prior to timing code you may want to
;; ensure things like:  there has just been a GC, the relevant code is
;; already loaded (so that there's no overhead from autoloading etc.),
;; and the code is compiled if appropriate (but see
;; `benchmark-run-compiled').

;;; Code:

(defmacro benchmark-elapse (&rest forms)
  "Return the time in seconds elapsed for execution of FORMS."
  (let ((t1 (make-symbol "t1"))
	(t2 (make-symbol "t2")))
    `(let (,t1 ,t2)
       (setq ,t1 (current-time))
       ,@forms
       (setq ,t2 (current-time))
       (float-time (time-subtract ,t2 ,t1)))))

(put 'benchmark-elapse 'edebug-form-spec t)
(put 'benchmark-elapse 'lisp-indent-function 0)

;;;###autoload
(defmacro benchmark-run (&optional repetitions &rest forms)
  "Time execution of FORMS.
If REPETITIONS is supplied as a number, run forms that many times,
accounting for the overhead of the resulting loop.  Otherwise run
FORMS once.
Return a list of the total elapsed time for execution, the number of
garbage collections that ran, and the time taken by garbage collection.
See also `benchmark-run-compiled'."
  (unless (natnump repetitions)
    (setq forms (cons repetitions forms)
	  repetitions 1))
  (let ((i (make-symbol "i"))
	(gcs (make-symbol "gcs"))
	(gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
	   (,gcs gcs-done))
       (list ,(if (> repetitions 1)
		  ;; Take account of the loop overhead.
		  `(- (benchmark-elapse (dotimes (,i ,repetitions)
					  ,@forms))
		      (benchmark-elapse (dotimes (,i ,repetitions))))
		`(benchmark-elapse ,@forms))
	     (- gcs-done ,gcs)
	     (- gc-elapsed ,gc)))))
(put 'benchmark-run 'edebug-form-spec t)
(put 'benchmark-run 'lisp-indent-function 2)

;;;###autoload
(defmacro benchmark-run-compiled (&optional repetitions &rest forms)
  "Time execution of compiled version of FORMS.
This is like `benchmark-run', but what is timed is a funcall of the
byte code obtained by wrapping FORMS in a `lambda' and compiling the
result.  The overhead of the `lambda's is accounted for."
  (unless (natnump repetitions)
    (setq forms (cons repetitions forms)
	  repetitions 1))
  (let ((i (make-symbol "i"))
	(gcs (make-symbol "gcs"))
	(gc (make-symbol "gc"))
	(code (byte-compile `(lambda () ,@forms)))
	(lambda-code (byte-compile `(lambda ()))))
    `(let ((,gc gc-elapsed)
	   (,gcs gcs-done))
       (list ,(if (> repetitions 1)
		  ;; Take account of the loop overhead.
		  `(- (benchmark-elapse (dotimes (,i ,repetitions)
					  (funcall ,code)))
		      (benchmark-elapse (dotimes (,i ,repetitions)
					  (funcall ,lambda-code))))
		`(benchmark-elapse (funcall ,code)))
	     (- gcs-done ,gcs) (- gc-elapsed ,gc)))))
(put 'benchmark-run-compiled 'edebug-form-spec t)
(put 'benchmark-run-compiled 'lisp-indent-function 2)

;;;###autoload
(defun benchmark (repetitions form)
  "Print the time taken for REPETITIONS executions of FORM.
Interactively, REPETITIONS is taken from the prefix arg.
For non-interactive use see also `benchmark-run' and
`benchmark-run-compiled'."
  (interactive "p\nxForm: ")
  (let ((result (eval `(benchmark-run ,repetitions ,form))))
    (if (zerop (nth 1 result))
	(message "Elapsed time: %fs" (car result))
      (message "Elapsed time: %fs (%fs in %d GCs)" (car result)
	       (nth 2 result) (nth 1 result)))))

(provide 'benchmark)

;;; benchmark.el ends here
