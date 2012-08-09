;;; timer.el --- run a function with args at some time in future

;; Copyright (C) 1996, 2001-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
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

;; This package gives you the capability to run Emacs Lisp commands at
;; specified times in the future, either as one-shots or periodically.

;;; Code:

;; Layout of a timer vector:
;; [triggered-p high-seconds low-seconds usecs repeat-delay
;;  function args idle-delay]
;; triggered-p is nil if the timer is active (waiting to be triggered),
;;  t if it is inactive ("already triggered", in theory)

(eval-when-compile (require 'cl))

(defstruct (timer
            (:constructor nil)
            (:copier nil)
            (:constructor timer-create ())
            (:type vector)
            (:conc-name timer--))
  (triggered t)
  high-seconds low-seconds usecs repeat-delay function args idle-delay)

(defun timerp (object)
  "Return t if OBJECT is a timer."
  (and (vectorp object) (= (length object) 8)))

;; Pseudo field `time'.
(defun timer--time (timer)
  (list (timer--high-seconds timer)
        (timer--low-seconds timer)
        (timer--usecs timer)))

(defsetf timer--time
  (lambda (timer time)
    (or (timerp timer) (error "Invalid timer"))
    (setf (timer--high-seconds timer) (pop time))
    (setf (timer--low-seconds timer)
	  (if (consp time) (car time) time))
    (setf (timer--usecs timer) (or (and (consp time) (consp (cdr time))
					(cadr time))
				   0))))


(defun timer-set-time (timer time &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be in the internal format returned by, e.g., `current-time'.
If optional third argument DELTA is a positive number, make the timer
fire repeatedly that many seconds apart."
  (setf (timer--time timer) time)
  (setf (timer--repeat-delay timer) (and (numberp delta) (> delta 0) delta))
  timer)

(defun timer-set-idle-time (timer secs &optional repeat)
  "Set the trigger idle time of TIMER to SECS.
SECS may be an integer, floating point number, or the internal
time format (HIGH LOW USECS) returned by, e.g., `current-idle-time'.
If optional third argument REPEAT is non-nil, make the timer
fire each time Emacs is idle for that many seconds."
  (if (consp secs)
      (setf (timer--time timer) secs)
    (setf (timer--time timer) '(0 0 0))
    (timer-inc-time timer secs))
  (setf (timer--repeat-delay timer) repeat)
  timer)

(defun timer-next-integral-multiple-of-time (time secs)
  "Yield the next value after TIME that is an integral multiple of SECS.
More precisely, the next value, after TIME, that is an integral multiple
of SECS seconds since the epoch.  SECS may be a fraction."
  (let ((time-base (ash 1 16)))
    ;; Use floating point, taking care to not lose precision.
    (let* ((float-time-base (float time-base))
	   (million 1000000.0)
	   (time-usec (+ (* million
			    (+ (* float-time-base (nth 0 time))
			       (nth 1 time)))
			 (nth 2 time)))
	   (secs-usec (* million secs))
	   (mod-usec (mod time-usec secs-usec))
	   (next-usec (+ (- time-usec mod-usec) secs-usec))
	   (time-base-million (* float-time-base million)))
      (list (floor next-usec time-base-million)
	    (floor (mod next-usec time-base-million) million)
	    (floor (mod next-usec million))))))

(defun timer-relative-time (time secs &optional usecs)
  "Advance TIME by SECS seconds and optionally USECS microseconds.
SECS may be either an integer or a floating point number."
  (let ((delta (if (floatp secs)
		   (seconds-to-time secs)
		 (list (floor secs 65536) (mod secs 65536)))))
    (if usecs
	(setq delta (time-add delta (list 0 0 usecs))))
    (time-add time delta)))

(defun timer--time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (time-less-p (timer--time t1) (timer--time t2)))

(defun timer-inc-time (timer secs &optional usecs)
  "Increment the time set in TIMER by SECS seconds and USECS microseconds.
SECS may be a fraction.  If USECS is omitted, that means it is zero."
  (setf (timer--time timer)
        (timer-relative-time (timer--time timer) secs usecs)))

(defun timer-set-time-with-usecs (timer time usecs &optional delta)
  "Set the trigger time of TIMER to TIME plus USECS.
TIME must be in the internal format returned by, e.g., `current-time'.
The microsecond count from TIME is ignored, and USECS is used instead.
If optional fourth argument DELTA is a positive number, make the timer
fire repeatedly that many seconds apart."
  (setf (timer--time timer) time)
  (setf (timer--usecs timer) usecs)
  (setf (timer--repeat-delay timer) (and (numberp delta) (> delta 0) delta))
  timer)
(make-obsolete 'timer-set-time-with-usecs
               "use `timer-set-time' and `timer-inc-time' instead."
               "22.1")

(defun timer-set-function (timer function &optional args)
  "Make TIMER call FUNCTION with optional ARGS when triggering."
  (or (timerp timer)
      (error "Invalid timer"))
  (setf (timer--function timer) function)
  (setf (timer--args timer) args)
  timer)

(defun timer--activate (timer &optional triggered-p reuse-cell idle)
  (if (and (timerp timer)
	   (integerp (timer--high-seconds timer))
	   (integerp (timer--low-seconds timer))
	   (integerp (timer--usecs timer))
	   (timer--function timer))
      (let ((timers (if idle timer-idle-list timer-list))
	    last)
	;; Skip all timers to trigger before the new one.
	(while (and timers (timer--time-less-p (car timers) timer))
	  (setq last timers
		timers (cdr timers)))
	(if reuse-cell
	    (progn
	      (setcar reuse-cell timer)
	      (setcdr reuse-cell timers))
	  (setq reuse-cell (cons timer timers)))
	;; Insert new timer after last which possibly means in front of queue.
	(cond (last (setcdr last reuse-cell))
	      (idle (setq timer-idle-list reuse-cell))
	      (t    (setq timer-list reuse-cell)))
	(setf (timer--triggered timer) triggered-p)
	(setf (timer--idle-delay timer) idle)
	nil)
    (error "Invalid or uninitialized timer")))

(defun timer-activate (timer &optional triggered-p reuse-cell)
  "Insert TIMER into `timer-list'.
If TRIGGERED-P is t, make TIMER inactive (put it on the list, but
mark it as already triggered).  To remove it, use `cancel-timer'.

REUSE-CELL, if non-nil, is a cons cell to reuse when inserting
TIMER into `timer-list' (usually a cell removed from that list by
`cancel-timer-internal'; using this reduces consing for repeat
timers).  If nil, allocate a new cell."
  (timer--activate timer triggered-p reuse-cell nil))

(defun timer-activate-when-idle (timer &optional dont-wait reuse-cell)
  "Insert TIMER into `timer-idle-list'.
This arranges to activate TIMER whenever Emacs is next idle.
If optional argument DONT-WAIT is non-nil, set TIMER to activate
immediately, or at the right time, if Emacs is already idle.

REUSE-CELL, if non-nil, is a cons cell to reuse when inserting
TIMER into `timer-idle-list' (usually a cell removed from that
list by `cancel-timer-internal'; using this reduces consing for
repeat timers).  If nil, allocate a new cell."
  (timer--activate timer (not dont-wait) reuse-cell 'idle))

(defalias 'disable-timeout 'cancel-timer)

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (or (timerp timer)
      (error "Invalid timer"))
  (setq timer-list (delq timer timer-list))
  (setq timer-idle-list (delq timer timer-idle-list))
  nil)

(defun cancel-timer-internal (timer)
  "Remove TIMER from the list of active timers or idle timers.
Only to be used in this file.  It returns the cons cell
that was removed from the timer list."
  (let ((cell1 (memq timer timer-list))
	(cell2 (memq timer timer-idle-list)))
    (if cell1
	(setq timer-list (delq timer timer-list)))
    (if cell2
	(setq timer-idle-list (delq timer timer-idle-list)))
    (or cell1 cell2)))

(defun cancel-function-timers (function)
  "Cancel all timers which would run FUNCTION.
This affects ordinary timers such as are scheduled by `run-at-time',
and idle timers such as are scheduled by `run-with-idle-timer'."
  (interactive "aCancel timers of function: ")
  (dolist (timer timer-list)
    (if (eq (timer--function timer) function)
        (setq timer-list (delq timer timer-list))))
  (dolist (timer timer-idle-list)
    (if (eq (timer--function timer) function)
        (setq timer-idle-list (delq timer timer-idle-list)))))

;; Record the last few events, for debugging.
(defvar timer-event-last nil
  "Last timer that was run.")
(defvar timer-event-last-1 nil
  "Next-to-last timer that was run.")
(defvar timer-event-last-2 nil
  "Third-to-last timer that was run.")

(defvar timer-max-repeats 10
  "*Maximum number of times to repeat a timer, if many repeats are delayed.
Timer invocations can be delayed because Emacs is suspended or busy,
or because the system's time changes.  If such an occurrence makes it
appear that many invocations are overdue, this variable controls
how many will really happen.")

(defun timer-until (timer time)
  "Calculate number of seconds from when TIMER will run, until TIME.
TIMER is a timer, and stands for the time when its next repeat is scheduled.
TIME is a time-list."
  (float-time (time-subtract time (timer--time timer))))

(defun timer-event-handler (timer)
  "Call the handler for the timer TIMER.
This function is called, by name, directly by the C code."
  (setq timer-event-last-2 timer-event-last-1)
  (setq timer-event-last-1 timer-event-last)
  (setq timer-event-last timer)
  (let ((inhibit-quit t))
    (if (timerp timer)
	(let (retrigger cell)
	  ;; Delete from queue.  Record the cons cell that was used.
	  (setq cell (cancel-timer-internal timer))
	  ;; Re-schedule if requested.
	  (if (timer--repeat-delay timer)
	      (if (timer--idle-delay timer)
		  (timer-activate-when-idle timer nil cell)
		(timer-inc-time timer (timer--repeat-delay timer) 0)
		;; If real time has jumped forward,
		;; perhaps because Emacs was suspended for a long time,
		;; limit how many times things get repeated.
		(if (and (numberp timer-max-repeats)
			 (< 0 (timer-until timer (current-time))))
		    (let ((repeats (/ (timer-until timer (current-time))
				      (timer--repeat-delay timer))))
		      (if (> repeats timer-max-repeats)
			  (timer-inc-time timer (* (timer--repeat-delay timer)
                                                   repeats)))))
		(timer-activate timer t cell)
		(setq retrigger t)))
	  ;; Run handler.
	  ;; We do this after rescheduling so that the handler function
	  ;; can cancel its own timer successfully with cancel-timer.
	  (condition-case nil
              ;; Timer functions should not change the current buffer.
              ;; If they do, all kinds of nasty surprises can happen,
              ;; and it can be hellish to track down their source.
              (save-current-buffer
                (apply (timer--function timer) (timer--args timer)))
	    (error nil))
	  (if retrigger
	      (setf (timer--triggered timer) nil)))
      (error "Bogus timer event"))))

;; This function is incompatible with the one in levents.el.
(defun timeout-event-p (event)
  "Non-nil if EVENT is a timeout event."
  (and (listp event) (eq (car event) 'timer-event)))


(declare-function diary-entry-time "diary-lib" (s))

(defun run-at-time (time repeat function &rest args)
  "Perform an action at time TIME.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
TIME should be one of: a string giving an absolute time like
\"11:23pm\" (the acceptable formats are those recognized by
`diary-entry-time'; note that such times are interpreted as times
today, even if in the past); a string giving a relative time like
\"2 hours 35 minutes\" (the acceptable formats are those
recognized by `timer-duration'); nil meaning now; a number of
seconds from now; a value from `encode-time'; or t (with non-nil
REPEAT) meaning the next integral multiple of REPEAT.  REPEAT may
be an integer or floating point number.  The action is to call
FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")

  (or (null repeat)
      (and (numberp repeat) (< 0 repeat))
      (error "Invalid repetition interval"))

  ;; Special case: nil means "now" and is useful when repeating.
  (if (null time)
      (setq time (current-time)))

  ;; Special case: t means the next integral multiple of REPEAT.
  (if (and (eq time t) repeat)
      (setq time (timer-next-integral-multiple-of-time (current-time) repeat)))

  ;; Handle numbers as relative times in seconds.
  (if (numberp time)
      (setq time (timer-relative-time (current-time) time)))

  ;; Handle relative times like "2 hours 35 minutes"
  (if (stringp time)
      (let ((secs (timer-duration time)))
	(if secs
	    (setq time (timer-relative-time (current-time) secs)))))

  ;; Handle "11:23pm" and the like.  Interpret it as meaning today
  ;; which admittedly is rather stupid if we have passed that time
  ;; already.  (Though only Emacs hackers hack Emacs at that time.)
  (if (stringp time)
      (progn
	(require 'diary-lib)
	(let ((hhmm (diary-entry-time time))
	      (now (decode-time)))
	  (if (>= hhmm 0)
	      (setq time
		    (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
				 (nth 4 now) (nth 5 now) (nth 8 now)))))))

  (or (consp time)
      (error "Invalid time format"))

  (let ((timer (timer-create)))
    (timer-set-time timer time repeat)
    (timer-set-function timer function args)
    (timer-activate timer)
    timer))

(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
SECS and REPEAT may be integers or floating point numbers.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive "sRun after delay (seconds): \nNRepeat interval: \naFunction: ")
  (apply 'run-at-time secs repeat function args))

(defun add-timeout (secs function object &optional repeat)
  "Add a timer to run SECS seconds from now, to call FUNCTION on OBJECT.
If REPEAT is non-nil, repeat the timer every REPEAT seconds.
This function is for compatibility; see also `run-with-timer'."
  (run-with-timer secs repeat function object))

(defun run-with-idle-timer (secs repeat function &rest args)
  "Perform an action the next time Emacs is idle for SECS seconds.
The action is to call FUNCTION with arguments ARGS.
SECS may be an integer, a floating point number, or the internal
time format (HIGH LOW USECS) returned by, e.g., `current-idle-time'.
If Emacs is currently idle, and has been idle for N seconds (N < SECS),
then it will call FUNCTION in SECS - N seconds from now.

If REPEAT is non-nil, do the action each time Emacs has been idle for
exactly SECS seconds (that is, only once for each time Emacs becomes idle).

This function returns a timer object which you can use in `cancel-timer'."
  (interactive
   (list (read-from-minibuffer "Run after idle (seconds): " nil nil t)
	 (y-or-n-p "Repeat each time Emacs is idle? ")
	 (intern (completing-read "Function: " obarray 'fboundp t))))
  (let ((timer (timer-create)))
    (timer-set-function timer function args)
    (timer-set-idle-time timer secs repeat)
    (timer-activate-when-idle timer t)
    timer))

(defvar with-timeout-timers nil
  "List of all timers used by currently pending `with-timeout' calls.")

(defmacro with-timeout (list &rest body)
  "Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The timeout is checked whenever Emacs waits for some kind of external
event (such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected.
\n(fn (SECONDS TIMEOUT-FORMS...) BODY)"
  (declare (indent 1) (debug ((form body) body)))
  (let ((seconds (car list))
	(timeout-forms (cdr list))
        (timeout (make-symbol "timeout")))
    `(let ((-with-timeout-value-
            (catch ',timeout
              (let* ((-with-timeout-timer-
                      (run-with-timer ,seconds nil
                                      (lambda () (throw ',timeout ',timeout))))
                     (with-timeout-timers
                         (cons -with-timeout-timer- with-timeout-timers)))
                (unwind-protect
                    ,@body
                  (cancel-timer -with-timeout-timer-))))))
       ;; It is tempting to avoid the `if' altogether and instead run
       ;; timeout-forms in the timer, just before throwing `timeout'.
       ;; But that would mean that timeout-forms are run in the deeper
       ;; dynamic context of the timer, with inhibit-quit set etc...
       (if (eq -with-timeout-value- ',timeout)
           (progn ,@timeout-forms)
         -with-timeout-value-))))

(defun with-timeout-suspend ()
  "Stop the clock for `with-timeout'.  Used by debuggers.
The idea is that the time you spend in the debugger should not
count against these timeouts.

The value is a list that the debugger can pass to `with-timeout-unsuspend'
when it exits, to make these timers start counting again."
  (mapcar (lambda (timer)
	    (cancel-timer timer)
	    (list timer (time-subtract (timer--time timer) (current-time))))
	  with-timeout-timers))

(defun with-timeout-unsuspend (timer-spec-list)
  "Restart the clock for `with-timeout'.
The argument should be a value previously returned by `with-timeout-suspend'."
  (dolist (elt timer-spec-list)
    (let ((timer (car elt))
	  (delay (cadr elt)))
      (timer-set-time timer (time-add (current-time) delay))
      (timer-activate timer))))

(defun y-or-n-p-with-timeout (prompt seconds default-value)
  "Like (y-or-n-p PROMPT), with a timeout.
If the user does not answer after SECONDS seconds, return DEFAULT-VALUE."
  (with-timeout (seconds default-value)
    (y-or-n-p prompt)))

(defconst timer-duration-words
  (list (cons "microsec" 0.000001)
	(cons "microsecond" 0.000001)
        (cons "millisec" 0.001)
	(cons "millisecond" 0.001)
        (cons "sec" 1)
	(cons "second" 1)
	(cons "min" 60)
	(cons "minute" 60)
	(cons "hour" (* 60 60))
	(cons "day" (* 24 60 60))
	(cons "week" (* 7 24 60 60))
	(cons "fortnight" (* 14 24 60 60))
	(cons "month" (* 30 24 60 60))	  ; Approximation
	(cons "year" (* 365.25 24 60 60)) ; Approximation
	)
  "Alist mapping temporal words to durations in seconds.")

(defun timer-duration (string)
  "Return number of seconds specified by STRING, or nil if parsing fails."
  (let ((secs 0)
	(start 0)
	(case-fold-search t))
    (while (string-match
	    "[ \t]*\\([0-9.]+\\)?[ \t]*\\([a-z]+[a-rt-z]\\)s?[ \t]*"
	    string start)
      (let ((count (if (match-beginning 1)
		       (string-to-number (match-string 1 string))
		     1))
	    (itemsize (cdr (assoc (match-string 2 string)
				  timer-duration-words))))
	(if itemsize
	    (setq start (match-end 0)
		  secs (+ secs (* count itemsize)))
	  (setq secs nil
		start (length string)))))
    (if (= start (length string))
	secs
      (if (string-match-p "\\`[0-9.]+\\'" string)
	  (string-to-number string)))))

(provide 'timer)

;;; timer.el ends here
