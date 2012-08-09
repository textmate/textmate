;;; timeclock.el --- mode for keeping track of how much you work

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 25 Mar 1999
;; Version: 2.6.1
;; Keywords: calendar data

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

;; This mode is for keeping track of time intervals.  You can use it
;; for whatever purpose you like, but the typical scenario is to keep
;; track of how much time you spend working on certain projects.
;;
;; Use `timeclock-in' when you start on a project, and `timeclock-out'
;; when you're done.  Once you've collected some data, you can use
;; `timeclock-workday-remaining' to see how much time is left to be
;; worked today (where `timeclock-workday' specifies the length of the
;; working day), and `timeclock-when-to-leave' to calculate when you're free.

;; You'll probably want to bind the timeclock commands to some handy
;; keystrokes.  At the moment, C-x t is unused:
;;
;;   (require 'timeclock)
;;
;;   (define-key ctl-x-map "ti" 'timeclock-in)
;;   (define-key ctl-x-map "to" 'timeclock-out)
;;   (define-key ctl-x-map "tc" 'timeclock-change)
;;   (define-key ctl-x-map "tr" 'timeclock-reread-log)
;;   (define-key ctl-x-map "tu" 'timeclock-update-modeline)
;;   (define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)

;; If you want Emacs to display the amount of time "left" to your
;; workday in the modeline, you can either set the value of
;; `timeclock-modeline-display' to t using M-x customize, or you
;; can add this code to your .emacs file:
;;
;;   (require 'timeclock)
;;   (timeclock-modeline-display)
;;
;; To cancel this modeline display at any time, just call
;; `timeclock-modeline-display' again.

;; You may also want Emacs to ask you before exiting, if you are
;; currently working on a project.  This can be done either by setting
;; `timeclock-ask-before-exiting' to t using M-x customize (this is
;; the default), or by adding the following to your .emacs file:
;;
;;   (add-hook 'kill-emacs-query-functions 'timeclock-query-out)

;; NOTE: If you change your .timelog file without using timeclock's
;; functions, or if you change the value of any of timeclock's
;; customizable variables, you should run the command
;; `timeclock-reread-log'.  This will recompute any discrepancies in
;; your average working time, and will make sure that the various
;; display functions return the correct value.

;;; History:

;;; Code:

(defgroup timeclock nil
  "Keeping track of the time that gets spent."
  :group 'data)

;;; User Variables:

(defcustom timeclock-file (convert-standard-filename "~/.timelog")
  "The file used to store timeclock data in."
  :type 'file
  :group 'timeclock)

(defcustom timeclock-workday (* 8 60 60)
  "The length of a work period in seconds."
  :type 'integer
  :group 'timeclock)

(defcustom timeclock-relative t
  "Whether to make reported time relative to `timeclock-workday'.
For example, if the length of a normal workday is eight hours, and you
work four hours on Monday, then the amount of time \"remaining\" on
Tuesday is twelve hours -- relative to an averaged work period of
eight hours -- or eight hours, non-relative.  So relative time takes
into account any discrepancy of time under-worked or over-worked on
previous days.  This only affects the timeclock modeline display."
  :type 'boolean
  :group 'timeclock)

(defcustom timeclock-get-project-function 'timeclock-ask-for-project
  "The function used to determine the name of the current project.
When clocking in, and no project is specified, this function will be
called to determine what is the current project to be worked on.
If this variable is nil, no questions will be asked."
  :type 'function
  :group 'timeclock)

(defcustom timeclock-get-reason-function 'timeclock-ask-for-reason
  "A function used to determine the reason for clocking out.
When clocking out, and no reason is specified, this function will be
called to determine what is the reason.
If this variable is nil, no questions will be asked."
  :type 'function
  :group 'timeclock)

(defcustom timeclock-get-workday-function nil
  "A function used to determine the length of today's workday.
The first time that a user clocks in each day, this function will be
called to determine what is the length of the current workday.  If
the return value is nil, or equal to `timeclock-workday', nothing special
will be done.  If it is a quantity different from `timeclock-workday',
however, a record will be output to the timelog file to note the fact that
that day has a length that is different from the norm."
  :type '(choice (const nil) function)
  :group 'timeclock)

(defcustom timeclock-ask-before-exiting t
  "If non-nil, ask if the user wants to clock out before exiting Emacs.
This variable only has effect if set with \\[customize]."
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'kill-emacs-query-functions 'timeclock-query-out)
	   (remove-hook 'kill-emacs-query-functions 'timeclock-query-out))
	 (setq timeclock-ask-before-exiting value))
  :type 'boolean
  :group 'timeclock)

(defvar timeclock-update-timer nil
  "The timer used to update `timeclock-mode-string'.")

;; For byte-compiler.
(defvar display-time-hook)
(defvar timeclock-modeline-display)

(defcustom timeclock-use-display-time t
  "If non-nil, use `display-time-hook' for doing modeline updates.
The advantage of this is that one less timer has to be set running
amok in Emacs's process space.  The disadvantage is that it requires
you to have `display-time' running.  If you don't want to use
`display-time', but still want the modeline to show how much time is
left, set this variable to nil.  Changing the value of this variable
while timeclock information is being displayed in the modeline has no
effect.  You should call the function `timeclock-modeline-display' with
a positive argument to force an update."
  :set (lambda (symbol value)
	 (let ((currently-displaying
		(and (boundp 'timeclock-modeline-display)
		     timeclock-modeline-display)))
	   ;; if we're changing to the state that
	   ;; `timeclock-modeline-display' is already using, don't
	   ;; bother toggling it.  This happens on the initial loading
	   ;; of timeclock.el.
	   (if (and currently-displaying
		    (or (and value
			     (boundp 'display-time-hook)
			     (memq 'timeclock-update-modeline
				   display-time-hook))
			(and (not value)
			     timeclock-update-timer)))
	       (setq currently-displaying nil))
	   (and currently-displaying
		(set-variable 'timeclock-modeline-display nil))
	   (setq timeclock-use-display-time value)
	   (and currently-displaying
		(set-variable 'timeclock-modeline-display t))
	   timeclock-use-display-time))
  :type 'boolean
  :group 'timeclock
  :require 'time)

(defcustom timeclock-first-in-hook nil
  "A hook run for the first \"in\" event each day.
Note that this hook is run before recording any events.  Thus the
value of `timeclock-hours-today', `timeclock-last-event' and the
return value of function `timeclock-last-period' are relative previous
to today."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-load-hook nil
  "Hook that gets run after timeclock has been loaded."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-in-hook nil
  "A hook run every time an \"in\" event is recorded."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-day-over-hook nil
  "A hook that is run when the workday has been completed.
This hook is only run if the current time remaining is being displayed
in the modeline.  See the variable `timeclock-modeline-display'."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-out-hook nil
  "A hook run every time an \"out\" event is recorded."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-done-hook nil
  "A hook run every time a project is marked as completed."
  :type 'hook
  :group 'timeclock)

(defcustom timeclock-event-hook nil
  "A hook run every time any event is recorded."
  :type 'hook
  :group 'timeclock)

(defvar timeclock-last-event nil
  "A list containing the last event that was recorded.
The format of this list is (CODE TIME PROJECT).")

(defvar timeclock-last-event-workday nil
  "The number of seconds in the workday of `timeclock-last-event'.")

;;; Internal Variables:

(defvar timeclock-discrepancy nil
  "A variable containing the time discrepancy before the last event.
Normally, timeclock assumes that you intend to work for
`timeclock-workday' seconds every day.  Any days in which you work
more or less than this amount is considered either a positive or
a negative discrepancy.  If you work in such a manner that the
discrepancy is always brought back to zero, then you will by
definition have worked an average amount equal to `timeclock-workday'
each day.")

(defvar timeclock-elapsed nil
  "A variable containing the time elapsed for complete periods today.
This value is not accurate enough to be useful by itself.  Rather,
call `timeclock-workday-elapsed', to determine how much time has been
worked so far today.  Also, if `timeclock-relative' is nil, this value
will be the same as `timeclock-discrepancy'.")

(defvar timeclock-use-elapsed nil
  "Non-nil if the modeline should display time elapsed, not remaining.")

(defvar timeclock-last-period nil
  "Integer representing the number of seconds in the last period.
Note that you shouldn't access this value, but instead should use the
function `timeclock-last-period'.")

(defvar timeclock-mode-string nil
  "The timeclock string (optionally) displayed in the modeline.
The time is bracketed by <> if you are clocked in, otherwise by [].")

(defvar timeclock-day-over nil
  "The date of the last day when notified \"day over\" for.")

;;; User Functions:

;;;###autoload
(defun timeclock-modeline-display (&optional arg)
  "Toggle display of the amount of time left today in the modeline.
If `timeclock-use-display-time' is non-nil (the default), then
the function `display-time-mode' must be active, and the modeline
will be updated whenever the time display is updated.  Otherwise,
the timeclock will use its own sixty second timer to do its
updating.  With prefix ARG, turn modeline display on if and only
if ARG is positive.  Returns the new status of timeclock modeline
display (non-nil means on)."
  (interactive "P")
  ;; cf display-time-mode.
  (setq timeclock-mode-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
		(not timeclock-modeline-display))))
    (if on-p
        (progn
          (or (memq 'timeclock-mode-string global-mode-string)
              (setq global-mode-string
                    (append global-mode-string '(timeclock-mode-string))))
	  (unless (memq 'timeclock-update-modeline timeclock-event-hook)
	    (add-hook 'timeclock-event-hook 'timeclock-update-modeline))
	  (when timeclock-update-timer
	    (cancel-timer timeclock-update-timer)
	    (setq timeclock-update-timer nil))
	  (if (boundp 'display-time-hook)
	      (remove-hook 'display-time-hook 'timeclock-update-modeline))
	  (if timeclock-use-display-time
              (progn
                ;; Update immediately so there is a visible change
                ;; on calling this function.
                (if display-time-mode (timeclock-update-modeline)
                  (message "Activate `display-time-mode' or turn off \
`timeclock-use-display-time' to see timeclock information"))
                (add-hook 'display-time-hook 'timeclock-update-modeline))
	    (setq timeclock-update-timer
		  (run-at-time nil 60 'timeclock-update-modeline))))
      (setq global-mode-string
	    (delq 'timeclock-mode-string global-mode-string))
      (remove-hook 'timeclock-event-hook 'timeclock-update-modeline)
      (if (boundp 'display-time-hook)
	  (remove-hook 'display-time-hook
		       'timeclock-update-modeline))
      (when timeclock-update-timer
	(cancel-timer timeclock-update-timer)
	(setq timeclock-update-timer nil)))
    (force-mode-line-update)
    (setq timeclock-modeline-display on-p)))

;; This has to be here so that the function definition of
;; `timeclock-modeline-display' is known to the "set" function.
(defcustom timeclock-modeline-display nil
  "Toggle modeline display of time remaining.
You must modify via \\[customize] for this variable to have an effect."
  :set (lambda (symbol value)
	 (setq timeclock-modeline-display
	       (timeclock-modeline-display (or value 0))))
  :type 'boolean
  :group 'timeclock
  :require 'timeclock)

(defsubst timeclock-time-to-date (time)
  "Convert the TIME value to a textual date string."
  (format-time-string "%Y/%m/%d" time))

;;;###autoload
(defun timeclock-in (&optional arg project find-project)
  "Clock in, recording the current time moment in the timelog.
With a numeric prefix ARG, record the fact that today has only that
many hours in it to be worked.  If ARG is a non-numeric prefix argument
\(non-nil, but not a number), 0 is assumed (working on a holiday or
weekend).  *If not called interactively, ARG should be the number of
_seconds_ worked today*.  This feature only has effect the first time
this function is called within a day.

PROJECT is the project being clocked into.  If PROJECT is nil, and
FIND-PROJECT is non-nil -- or the user calls `timeclock-in'
interactively -- call the function `timeclock-get-project-function' to
discover the name of the project."
  (interactive
   (list (and current-prefix-arg
	      (if (numberp current-prefix-arg)
		  (* current-prefix-arg 60 60)
		0))))
  (if (equal (car timeclock-last-event) "i")
      (error "You've already clocked in!")
    (unless timeclock-last-event
      (timeclock-reread-log))
    ;; Either no log file, or day has rolled over.
    (unless (and timeclock-last-event
                 (equal (timeclock-time-to-date
                         (cadr timeclock-last-event))
                        (timeclock-time-to-date (current-time))))
      (let ((workday (or (and (numberp arg) arg)
			 (and arg 0)
			 (and timeclock-get-workday-function
			      (funcall timeclock-get-workday-function))
			 timeclock-workday)))
	(run-hooks 'timeclock-first-in-hook)
	;; settle the discrepancy for the new day
	(setq timeclock-discrepancy
	      (- (or timeclock-discrepancy 0) workday))
	(if (not (= workday timeclock-workday))
	    (timeclock-log "h" (number-to-string
				(/ workday (if (zerop (% workday (* 60 60)))
					       60 60.0) 60))))))
    (timeclock-log "i" (or project
			   (and timeclock-get-project-function
				(or find-project
				    (called-interactively-p 'interactive))
				(funcall timeclock-get-project-function))))
    (run-hooks 'timeclock-in-hook)))

;;;###autoload
(defun timeclock-out (&optional arg reason find-reason)
  "Clock out, recording the current time moment in the timelog.
If a prefix ARG is given, the user has completed the project that was
begun during the last time segment.

REASON is the user's reason for clocking out.  If REASON is nil, and
FIND-REASON is non-nil -- or the user calls `timeclock-out'
interactively -- call the function `timeclock-get-reason-function' to
discover the reason."
  (interactive "P")
  (or timeclock-last-event
      (error "You haven't clocked in!"))
  (if (equal (downcase (car timeclock-last-event)) "o")
      (error "You've already clocked out!")
    (timeclock-log
     (if arg "O" "o")
     (or reason
	 (and timeclock-get-reason-function
	      (or find-reason (called-interactively-p 'interactive))
	      (funcall timeclock-get-reason-function))))
    (run-hooks 'timeclock-out-hook)
    (if arg
	(run-hooks 'timeclock-done-hook))))

;; Should today-only be removed in favor of timeclock-relative? - gm
(defsubst timeclock-workday-remaining (&optional today-only)
  "Return the number of seconds until the workday is complete.
The amount returned is relative to the value of `timeclock-workday'.
If TODAY-ONLY is non-nil, the value returned will be relative only to
the time worked today, and not to past time."
  (let ((discrep (timeclock-find-discrep)))
    (if discrep
        (- (if today-only (cadr discrep)
             (car discrep)))
      0.0)))

;;;###autoload
(defun timeclock-status-string (&optional show-seconds today-only)
  "Report the overall timeclock status at the present moment.
If SHOW-SECONDS is non-nil, display second resolution.
If TODAY-ONLY is non-nil, the display will be relative only to time
worked today, ignoring the time worked on previous days."
  (interactive "P")
  (let ((remainder (timeclock-workday-remaining
		    (or today-only
			(not timeclock-relative))))
        (last-in (equal (car timeclock-last-event) "i"))
        status)
    (setq status
	  (format "Currently %s since %s (%s), %s %s, leave at %s"
		  (if last-in "IN" "OUT")
		  (if show-seconds
		      (format-time-string "%-I:%M:%S %p"
					  (nth 1 timeclock-last-event))
		    (format-time-string "%-I:%M %p"
					(nth 1 timeclock-last-event)))
		  (or (nth 2 timeclock-last-event)
		      (if last-in "**UNKNOWN**" "workday over"))
		  (timeclock-seconds-to-string remainder show-seconds t)
		  (if (> remainder 0)
		      "remaining" "over")
		  (timeclock-when-to-leave-string show-seconds today-only)))
    (if (called-interactively-p 'interactive)
	(message "%s" status)
      status)))

;;;###autoload
(defun timeclock-change (&optional arg project)
  "Change to working on a different project.
This clocks out of the current project, then clocks in on a new one.
With a prefix ARG, consider the previous project as finished at the
time of changeover.  PROJECT is the name of the last project you were
working on."
  (interactive "P")
  (timeclock-out arg)
  (timeclock-in nil project (called-interactively-p 'interactive)))

;;;###autoload
(defun timeclock-query-out ()
  "Ask the user whether to clock out.
This is a useful function for adding to `kill-emacs-query-functions'."
  (and (equal (car timeclock-last-event) "i")
       (y-or-n-p "You're currently clocking time, clock out? ")
       (timeclock-out))
  ;; Unconditionally return t for `kill-emacs-query-functions'.
  t)

;;;###autoload
(defun timeclock-reread-log ()
  "Re-read the timeclock, to account for external changes.
Returns the new value of `timeclock-discrepancy'."
  (interactive)
  (setq timeclock-discrepancy nil)
  (timeclock-find-discrep)
  (if (and timeclock-discrepancy timeclock-modeline-display)
      (timeclock-update-modeline))
  timeclock-discrepancy)

(defun timeclock-seconds-to-string (seconds &optional show-seconds
					    reverse-leader)
  "Convert SECONDS into a compact time string.
If SHOW-SECONDS is non-nil, make the resolution of the return string
include the second count.  If REVERSE-LEADER is non-nil, it means to
output a \"+\" if the time value is negative, rather than a \"-\".
This is used when negative time values have an inverted meaning (such
as with time remaining, where negative time really means overtime)."
  (if show-seconds
      (format "%s%d:%02d:%02d"
	      (if (< seconds 0) (if reverse-leader "+" "-") "")
	      (truncate (/ (abs seconds) 60 60))
	      (% (truncate (/ (abs seconds) 60)) 60)
	      (% (truncate (abs seconds)) 60))
    (format "%s%d:%02d"
	    (if (< seconds 0) (if reverse-leader "+" "-") "")
	    (truncate (/ (abs seconds) 60 60))
	    (% (truncate (/ (abs seconds) 60)) 60))))

(defsubst timeclock-currently-in-p ()
  "Return non-nil if the user is currently clocked in."
  (equal (car timeclock-last-event) "i"))

;;;###autoload
(defun timeclock-workday-remaining-string (&optional show-seconds
						     today-only)
  "Return a string representing the amount of time left today.
Display second resolution if SHOW-SECONDS is non-nil.  If TODAY-ONLY
is non-nil, the display will be relative only to time worked today.
See `timeclock-relative' for more information about the meaning of
\"relative to today\"."
  (interactive)
  (let ((string (timeclock-seconds-to-string
		 (timeclock-workday-remaining today-only)
		 show-seconds t)))
    (if (called-interactively-p 'interactive)
	(message "%s" string)
      string)))

(defsubst timeclock-workday-elapsed ()
  "Return the number of seconds worked so far today.
If RELATIVE is non-nil, the amount returned will be relative to past
time worked.  The default is to return only the time that has elapsed
so far today."
  (let ((discrep (timeclock-find-discrep)))
    (if discrep
	(nth 2 discrep)
      0.0)))

;;;###autoload
(defun timeclock-workday-elapsed-string (&optional show-seconds)
  "Return a string representing the amount of time worked today.
Display seconds resolution if SHOW-SECONDS is non-nil.  If RELATIVE is
non-nil, the amount returned will be relative to past time worked."
  (interactive)
  (let ((string (timeclock-seconds-to-string (timeclock-workday-elapsed)
					     show-seconds)))
    (if (called-interactively-p 'interactive)
	(message "%s" string)
      string)))

(defalias 'timeclock-time-to-seconds (if (fboundp 'float-time) 'float-time
				       'time-to-seconds))

(defalias 'timeclock-seconds-to-time 'seconds-to-time)

;; Should today-only be removed in favor of timeclock-relative? - gm
(defsubst timeclock-when-to-leave (&optional today-only)
  "Return a time value representing the end of today's workday.
If TODAY-ONLY is non-nil, the value returned will be relative only to
the time worked today, and not to past time."
  (timeclock-seconds-to-time
   (- (timeclock-time-to-seconds (current-time))
      (let ((discrep (timeclock-find-discrep)))
	(if discrep
	    (if today-only
		(cadr discrep)
	      (car discrep))
	  0.0)))))

;;;###autoload
(defun timeclock-when-to-leave-string (&optional show-seconds
						 today-only)
  "Return a string representing the end of today's workday.
This string is relative to the value of `timeclock-workday'.  If
SHOW-SECONDS is non-nil, the value printed/returned will include
seconds.  If TODAY-ONLY is non-nil, the value returned will be
relative only to the time worked today, and not to past time."
  ;; Should today-only be removed in favor of timeclock-relative? - gm
  (interactive)
  (let* ((then (timeclock-when-to-leave today-only))
	 (string
	  (if show-seconds
	      (format-time-string "%-I:%M:%S %p" then)
	    (format-time-string "%-I:%M %p" then))))
    (if (called-interactively-p 'interactive)
	(message "%s" string)
      string)))

(defun timeclock-make-hours-explicit (old-default)
  "Specify all workday lengths in `timeclock-file'.
OLD-DEFAULT hours are set for every day that has no number indicated."
  (interactive "P")
  (if old-default (setq old-default (prefix-numeric-value old-default))
    (error "`timelog-make-hours-explicit' requires an explicit argument"))
  (let ((extant-timelog (find-buffer-visiting timeclock-file))
	current-date)
    (with-current-buffer (find-file-noselect timeclock-file t)
      (unwind-protect
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char (point-min))
	      (while (progn (skip-chars-forward "\n") (not (eobp)))
		;; This is just a variant of `timeclock-moment-regexp'.
		(unless (looking-at
			 (concat "^\\([bhioO]\\) \\([0-9]+/[0-9]+/[0-9]+\\) "
				 "\\([0-9]+:[0-9]+:[0-9]+\\)"))
		  (error "Can't parse `%s'" timeclock-file))
		(let ((this-date (match-string 2)))
		  (unless (or (and current-date
				   (string= this-date current-date))
			      (string= (match-string 1) "h"))
		    (insert (format "h %s %s %s\n" (match-string 2)
				    (match-string 3) old-default)))
		  (if (string-match "^[ih]" (match-string 1)) ; ignore logouts
		      (setq current-date this-date)))
		(forward-line))
	      (save-buffer)))
	(unless extant-timelog (kill-buffer (current-buffer)))))))

;;; Internal Functions:

(defvar timeclock-project-list nil)
(defvar timeclock-last-project nil)

(defun timeclock-completing-read (prompt alist &optional default)
  "A version of `completing-read' that works on both Emacs and XEmacs.
PROMPT, ALIST and DEFAULT are used for the PROMPT, COLLECTION and DEF
arguments of `completing-read'."
  (if (featurep 'xemacs)
      (let ((str (completing-read prompt alist)))
	(if (or (null str) (zerop (length str)))
	    default
	  str))
    (completing-read prompt alist nil nil nil nil default)))

(defun timeclock-ask-for-project ()
  "Ask the user for the project they are clocking into."
  (timeclock-completing-read
   (format "Clock into which project (default %s): "
	   (or timeclock-last-project
	       (car timeclock-project-list)))
   (mapcar 'list timeclock-project-list)
   (or timeclock-last-project
       (car timeclock-project-list))))

(defvar timeclock-reason-list nil)

(defun timeclock-ask-for-reason ()
  "Ask the user for the reason they are clocking out."
  (timeclock-completing-read "Reason for clocking out: "
			     (mapcar 'list timeclock-reason-list)))

(defun timeclock-update-modeline ()
  "Update the `timeclock-mode-string' displayed in the modeline.
The value of `timeclock-relative' affects the display as described in
that variable's documentation."
  (interactive)
  (let ((remainder
	 (if timeclock-use-elapsed
	     (timeclock-workday-elapsed)
	   (timeclock-workday-remaining (not timeclock-relative))))
        (last-in (equal (car timeclock-last-event) "i")))
    (when (and (< remainder 0)
	       (not (and timeclock-day-over
			 (equal timeclock-day-over
				(timeclock-time-to-date
				 (current-time))))))
      (setq timeclock-day-over
	    (timeclock-time-to-date (current-time)))
      (run-hooks 'timeclock-day-over-hook))
    (setq timeclock-mode-string
          (propertize
           (format " %c%s%c "
                   (if last-in ?< ?[)
 		   (timeclock-seconds-to-string remainder nil t)
		   (if last-in ?> ?]))
           'help-echo "timeclock: time remaining"))))

(put 'timeclock-mode-string 'risky-local-variable t)

(defun timeclock-log (code &optional project)
  "Log the event CODE to the timeclock log, at the time of call.
If PROJECT is a string, it represents the project which the event is
being logged for.  Normally only \"in\" events specify a project."
  (let ((extant-timelog (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-max))
	  (if (not (bolp))
	      (insert "\n"))
	  (let ((now (current-time)))
	    (insert code " "
		    (format-time-string "%Y/%m/%d %H:%M:%S" now)
		    (or (and (stringp project)
			     (> (length project) 0)
			     (concat " " project))
			"")
		    "\n")
	    (if (equal (downcase code) "o")
		(setq timeclock-last-period
		      (- (timeclock-time-to-seconds now)
			 (timeclock-time-to-seconds
			  (cadr timeclock-last-event)))
		      timeclock-discrepancy
		      (+ timeclock-discrepancy
			 timeclock-last-period)))
	    (setq timeclock-last-event (list code now project)))))
      (save-buffer)
      (unless extant-timelog (kill-buffer (current-buffer)))))
  (run-hooks 'timeclock-event-hook))

(defvar timeclock-moment-regexp
  (concat "\\([bhioO]\\)\\s-+"
	  "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)\\s-+"
	  "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)[ \t]*" "\\([^\n]*\\)"))

(defsubst timeclock-read-moment ()
  "Read the moment under point from the timelog."
  (if (looking-at timeclock-moment-regexp)
      (let ((code (match-string 1))
	    (year (string-to-number (match-string 2)))
	    (mon  (string-to-number (match-string 3)))
	    (mday (string-to-number (match-string 4)))
	    (hour (string-to-number (match-string 5)))
	    (min  (string-to-number (match-string 6)))
	    (sec  (string-to-number (match-string 7)))
	    (project (match-string 8)))
	(list code (encode-time sec min hour mday mon year) project))))

(defun timeclock-last-period (&optional moment)
  "Return the value of the last event period.
If the last event was a clock-in, the period will be open ended, and
growing every second.  Otherwise, it is a fixed amount which has been
recorded to disk.  If MOMENT is non-nil, use that as the current time.
This is only provided for coherency when used by
`timeclock-discrepancy'."
  (if (equal (car timeclock-last-event) "i")
      (- (timeclock-time-to-seconds (or moment (current-time)))
	 (timeclock-time-to-seconds
	  (cadr timeclock-last-event)))
    timeclock-last-period))

(defsubst timeclock-entry-length (entry)
  "Return the length of ENTRY in seconds."
  (- (timeclock-time-to-seconds (cadr entry))
     (timeclock-time-to-seconds (car entry))))

(defsubst timeclock-entry-begin (entry)
  "Return the start time of ENTRY."
  (car entry))

(defsubst timeclock-entry-end (entry)
  "Return the end time of ENTRY."
  (cadr entry))

(defsubst timeclock-entry-project (entry)
  "Return the project of ENTRY."
  (nth 2 entry))

(defsubst timeclock-entry-comment (entry)
  "Return the comment of ENTRY."
  (nth 3 entry))

(defsubst timeclock-entry-list-length (entry-list)
  "Return the total length of ENTRY-LIST in seconds."
  (let ((length 0))
    (dolist (entry entry-list)
      (setq length (+ length (timeclock-entry-length entry))))
    length))

(defsubst timeclock-entry-list-begin (entry-list)
  "Return the start time of the first element of ENTRY-LIST."
  (timeclock-entry-begin (car entry-list)))

(defsubst timeclock-entry-list-end (entry-list)
  "Return the end time of the last element of ENTRY-LIST."
  (timeclock-entry-end (car (last entry-list))))

(defsubst timeclock-entry-list-span (entry-list)
  "Return the total time in seconds spanned by ENTRY-LIST."
  (- (timeclock-time-to-seconds (timeclock-entry-list-end entry-list))
     (timeclock-time-to-seconds (timeclock-entry-list-begin entry-list))))

(defsubst timeclock-entry-list-break (entry-list)
  "Return the total break time (span - length) in ENTRY-LIST."
  (- (timeclock-entry-list-span entry-list)
     (timeclock-entry-list-length entry-list)))

(defsubst timeclock-entry-list-projects (entry-list)
  "Return a list of all the projects in ENTRY-LIST."
  (let (projects proj)
    (dolist (entry entry-list)
      (setq proj (timeclock-entry-project entry))
      (if projects
	  (add-to-list 'projects proj)
	(setq projects (list proj))))
    projects))

(defsubst timeclock-day-required (day)
  "Return the required length of DAY in seconds, default `timeclock-workday'."
  (or (car day) timeclock-workday))

(defsubst timeclock-day-length (day)
  "Return the actual length of DAY in seconds."
  (timeclock-entry-list-length (cdr day)))

(defsubst timeclock-day-debt (day)
  "Return the debt (required - actual) associated with DAY, in seconds."
  (- (timeclock-day-required day)
     (timeclock-day-length day)))

(defsubst timeclock-day-begin (day)
  "Return the start time of DAY."
  (timeclock-entry-list-begin (cdr day)))

(defsubst timeclock-day-end (day)
  "Return the end time of DAY."
  (timeclock-entry-list-end (cdr day)))

(defsubst timeclock-day-span (day)
  "Return the span of DAY."
  (timeclock-entry-list-span (cdr day)))

(defsubst timeclock-day-break (day)
  "Return the total break time of DAY."
  (timeclock-entry-list-break (cdr day)))

(defsubst timeclock-day-projects (day)
  "Return a list of all the projects in DAY."
  (timeclock-entry-list-projects (cddr day)))

(defmacro timeclock-day-list-template (func)
  "Template for summing the result of FUNC on each element of DAY-LIST."
  `(let ((length 0))
     (while day-list
       (setq length (+ length (,(eval func) (car day-list)))
	     day-list (cdr day-list)))
     length))

(defun timeclock-day-list-required (day-list)
  "Return total required length of DAY-LIST, in seconds."
  (timeclock-day-list-template 'timeclock-day-required))

(defun timeclock-day-list-length (day-list)
  "Return actual length of DAY-LIST, in seconds."
  (timeclock-day-list-template 'timeclock-day-length))

(defun timeclock-day-list-debt (day-list)
  "Return total debt (required - actual) of DAY-LIST."
  (timeclock-day-list-template 'timeclock-day-debt))

(defsubst timeclock-day-list-begin (day-list)
  "Return the start time of DAY-LIST."
  (timeclock-day-begin (car day-list)))

(defsubst timeclock-day-list-end (day-list)
  "Return the end time of DAY-LIST."
  (timeclock-day-end (car (last day-list))))

(defun timeclock-day-list-span (day-list)
  "Return the span of DAY-LIST."
  (timeclock-day-list-template 'timeclock-day-span))

(defun timeclock-day-list-break (day-list)
  "Return the total break of DAY-LIST."
  (timeclock-day-list-template 'timeclock-day-break))

(defun timeclock-day-list-projects (day-list)
  "Return a list of all the projects in DAY-LIST."
  (let (projects)
    (dolist (day day-list)
      (dolist (proj (timeclock-day-projects day))
	(if projects
	    (add-to-list 'projects proj)
	  (setq projects (list proj)))))
    projects))

(defsubst timeclock-current-debt (&optional log-data)
  "Return the seconds debt from LOG-DATA, default `timeclock-log-data'."
  (nth 0 (or log-data (timeclock-log-data))))

(defsubst timeclock-day-alist (&optional log-data)
  "Return the date alist from LOG-DATA, default `timeclock-log-data'."
  (nth 1 (or log-data (timeclock-log-data))))

(defun timeclock-day-list (&optional log-data)
  "Return a list of the cdrs of the date alist from LOG-DATA."
  (let (day-list)
    (dolist (date-list (timeclock-day-alist log-data))
      (setq day-list (cons (cdr date-list) day-list)))
    day-list))

(defsubst timeclock-project-alist (&optional log-data)
  "Return the project alist from LOG-DATA, default `timeclock-log-data'."
  (nth 2 (or log-data (timeclock-log-data))))

(defun timeclock-log-data (&optional recent-only filename)
  "Return the contents of the timelog file, in a useful format.
If the optional argument RECENT-ONLY is non-nil, only show the contents
from the last point where the time debt (see below) was set.
If the optional argument FILENAME is non-nil, it is used instead of
the file specified by `timeclock-file.'

A timelog contains data in the form of a single entry per line.
Each entry has the form:

  CODE YYYY/MM/DD HH:MM:SS [COMMENT]

CODE is one of: b, h, i, o or O.  COMMENT is optional when the code is
i, o or O.  The meanings of the codes are:

  b  Set the current time balance, or \"time debt\".  Useful when
     archiving old log data, when a debt must be carried forward.
     The COMMENT here is the number of seconds of debt.

  h  Set the required working time for the given day.  This must
     be the first entry for that day.  The COMMENT in this case is
     the number of hours in this workday.  Floating point amounts
     are allowed.

  i  Clock in.  The COMMENT in this case should be the name of the
     project worked on.

  o  Clock out.  COMMENT is unnecessary, but can be used to provide
     a description of how the period went, for example.

  O  Final clock out.  Whatever project was being worked on, it is
     now finished.  Useful for creating summary reports.

When this function is called, it will return a data structure with the
following format:

  (DEBT ENTRIES-BY-DAY ENTRIES-BY-PROJECT)

DEBT is a floating point number representing the number of seconds
\"owed\" before any work was done.  For a new file (one without a 'b'
entry), this is always zero.

The two entries lists have similar formats.  They are both alists,
where the CAR is the index, and the CDR is a list of time entries.
For ENTRIES-BY-DAY, the CAR is a textual date string, of the form
YYYY/MM/DD.  For ENTRIES-BY-PROJECT, it is the name of the project
worked on, or t for the default project.

The CDR for ENTRIES-BY-DAY is slightly different than for
ENTRIES-BY-PROJECT.  It has the following form:

  (DAY-LENGTH TIME-ENTRIES...)

For ENTRIES-BY-PROJECT, there is no DAY-LENGTH member.  It is simply a
list of TIME-ENTRIES.  Note that if DAY-LENGTH is nil, it means
whatever is the default should be used.

A TIME-ENTRY is a recorded time interval.  It has the following format
\(although generally one does not have to manipulate these entries
directly; see below):

  (BEGIN-TIME END-TIME PROJECT [COMMENT] [FINAL-P])

Anyway, suffice it to say there are a lot of structures.  Typically
the user is expected to manipulate to the day(s) or project(s) that he
or she wants, at which point the following helper functions may be
used:

  timeclock-day-required
  timeclock-day-length
  timeclock-day-debt
  timeclock-day-begin
  timeclock-day-end
  timeclock-day-span
  timeclock-day-break
  timeclock-day-projects

  timeclock-day-list-required
  timeclock-day-list-length
  timeclock-day-list-debt
  timeclock-day-list-begin
  timeclock-day-list-end
  timeclock-day-list-span
  timeclock-day-list-break
  timeclock-day-list-projects

  timeclock-entry-length
  timeclock-entry-begin
  timeclock-entry-end
  timeclock-entry-project
  timeclock-entry-comment

  timeclock-entry-list-length
  timeclock-entry-list-begin
  timeclock-entry-list-end
  timeclock-entry-list-span
  timeclock-entry-list-break
  timeclock-entry-list-projects

A few comments should make the use of the above functions obvious:

  `required' is the amount of time that must be spent during a day, or
  sequence of days, in order to have no debt.

  `length' is the actual amount of time that was spent.

  `debt' is the difference between required time and length.  A
  negative debt signifies overtime.

  `begin' is the earliest moment at which work began.

  `end' is the final moment work was done.

  `span' is the difference between begin and end.

  `break' is the difference between span and length.

  `project' is the project that was worked on, and `projects' is a
  list of all the projects that were worked on during a given period.

  `comment', where it applies, could mean anything.

There are a few more functions available, for locating day and entry
lists:

  timeclock-day-alist LOG-DATA
  timeclock-project-alist LOG-DATA
  timeclock-current-debt LOG-DATA

See the documentation for the given function if more info is needed."
  (let ((log-data (list 0.0 nil nil))
	(now (current-time))
	last-date-limited last-date-seconds last-date
	(line 0) last beg day entry event)
    (with-temp-buffer
      (insert-file-contents (or filename timeclock-file))
      (when recent-only
	(goto-char (point-max))
	(unless (re-search-backward "^b\\s-+" nil t)
	  (goto-char (point-min))))
      (while (or (setq event (timeclock-read-moment))
		 (and beg (not last)
		      (setq last t event (list "o" now))))
	(setq line (1+ line))
	(cond ((equal (car event) "b")
	       (setcar log-data (string-to-number (nth 2 event))))
	      ((equal (car event) "h")
	       (setq last-date-limited (timeclock-time-to-date (cadr event))
		     last-date-seconds (* (string-to-number (nth 2 event))
					  3600.0)))
	      ((equal (car event) "i")
	       (if beg
		   (error "Error in format of timelog file, line %d" line)
		 (setq beg t))
	       (setq entry (list (cadr event) nil
				 (and (> (length (nth 2 event)) 0)
				      (nth 2 event))))
	       (let ((date (timeclock-time-to-date (cadr event))))
		 (if (and last-date
			  (not (equal date last-date)))
		     (progn
		       (setcar (cdr log-data)
			       (cons (cons last-date day)
				     (cadr log-data)))
		       (setq day (list (and last-date-limited
					    last-date-seconds))))
		   (unless day
		     (setq day (list (and last-date-limited
					  last-date-seconds)))))
		 (setq last-date date
		       last-date-limited nil)))
	      ((equal (downcase (car event)) "o")
	       (if (not beg)
		   (error "Error in format of timelog file, line %d" line)
		 (setq beg nil))
	       (setcar (cdr entry) (cadr event))
	       (let ((desc (and (> (length (nth 2 event)) 0)
				(nth 2 event))))
		 (if desc
		     (nconc entry (list (nth 2 event))))
		 (if (equal (car event) "O")
		     (nconc entry (if desc
				      (list t)
				    (list nil t))))
		 (nconc day (list entry))
		 (setq desc (nth 2 entry))
		 (let ((proj (assoc desc (nth 2 log-data))))
		   (if (null proj)
		       (setcar (cddr log-data)
			       (cons (cons desc (list entry))
				     (nth 2 log-data)))
		     (nconc (cdr proj) (list entry)))))))
	(forward-line))
      (if day
	  (setcar (cdr log-data)
		  (cons (cons last-date day)
			(cadr log-data))))
      log-data)))

(defun timeclock-find-discrep ()
  "Calculate time discrepancies, in seconds.
The result is a three element list, containing the total time
discrepancy, today's discrepancy, and the time worked today."
  ;; This is not implemented in terms of the functions above, because
  ;; it's a bit wasteful to read all of that data in, just to throw
  ;; away more than 90% of the information afterwards.
  ;;
  ;; If it were implemented using those functions, it would look
  ;; something like this:
  ;;  (let ((days (timeclock-day-alist (timeclock-log-data)))
  ;;        (total 0.0))
  ;;    (while days
  ;;      (setq total (+ total (- (timeclock-day-length (cdar days))
  ;;                              (timeclock-day-required (cdar days))))
  ;;            days (cdr days)))
  ;;    total)
  (let* ((now (current-time))
	 (todays-date (timeclock-time-to-date now))
	 (first t) (accum 0) (elapsed 0)
	 event beg last-date
	 last-date-limited last-date-seconds)
    (unless timeclock-discrepancy
      (when (file-readable-p timeclock-file)
	(setq timeclock-project-list nil
	      timeclock-last-project nil
	      timeclock-reason-list nil
	      timeclock-elapsed 0)
	(with-temp-buffer
	  (insert-file-contents timeclock-file)
	  (goto-char (point-max))
	  (unless (re-search-backward "^b\\s-+" nil t)
	    (goto-char (point-min)))
	  (while (setq event (timeclock-read-moment))
	    (cond ((equal (car event) "b")
		   (setq accum (string-to-number (nth 2 event))))
		  ((equal (car event) "h")
		   (setq last-date-limited
			 (timeclock-time-to-date (cadr event))
			 last-date-seconds
			 (* (string-to-number (nth 2 event)) 3600.0)))
		  ((equal (car event) "i")
		   (when (and (nth 2 event)
			      (> (length (nth 2 event)) 0))
		     (add-to-list 'timeclock-project-list (nth 2 event))
		     (setq timeclock-last-project (nth 2 event)))
		   (let ((date (timeclock-time-to-date (cadr event))))
		     (if (if last-date
			     (not (equal date last-date))
			   first)
			 (setq first nil
			       accum (- accum (if last-date-limited
						  last-date-seconds
						timeclock-workday))))
		     (setq last-date date
			   last-date-limited nil)
		     (if beg
			 (error "Error in format of timelog file!")
		       (setq beg (timeclock-time-to-seconds (cadr event))))))
		  ((equal (downcase (car event)) "o")
		   (if (and (nth 2 event)
			    (> (length (nth 2 event)) 0))
		       (add-to-list 'timeclock-reason-list (nth 2 event)))
		   (if (not beg)
		       (error "Error in format of timelog file!")
		     (setq timeclock-last-period
			   (- (timeclock-time-to-seconds (cadr event)) beg)
			   accum (+ timeclock-last-period accum)
			   beg nil))
		   (if (equal last-date todays-date)
		       (setq timeclock-elapsed
			     (+ timeclock-last-period timeclock-elapsed)))))
	    (setq timeclock-last-event event
		  timeclock-last-event-workday
		  (if (equal (timeclock-time-to-date now) last-date-limited)
		      last-date-seconds
		    timeclock-workday))
	    (forward-line))
	  (setq timeclock-discrepancy accum))))
    (unless timeclock-last-event-workday
      (setq timeclock-last-event-workday timeclock-workday))
    (setq accum (or timeclock-discrepancy 0)
	  elapsed (or timeclock-elapsed elapsed))
    (if timeclock-last-event
	(if (equal (car timeclock-last-event) "i")
	    (let ((last-period (timeclock-last-period now)))
	      (setq accum (+ accum last-period)
		    elapsed (+ elapsed last-period)))
	  (if (not (equal (timeclock-time-to-date
			   (cadr timeclock-last-event))
			  (timeclock-time-to-date now)))
	      (setq accum (- accum timeclock-last-event-workday)))))
    (list accum (- elapsed timeclock-last-event-workday)
	  elapsed)))

;;; A reporting function that uses timeclock-log-data

(defun timeclock-day-base (&optional time)
  "Given a time within a day, return 0:0:0 within that day.
If optional argument TIME is non-nil, use that instead of the current time."
  (let ((decoded (decode-time (or time (current-time)))))
    (setcar (nthcdr 0 decoded) 0)
    (setcar (nthcdr 1 decoded) 0)
    (setcar (nthcdr 2 decoded) 0)
    (apply 'encode-time decoded)))

(defun timeclock-mean (l)
  "Compute the arithmetic mean of the values in the list L."
  (let ((total 0)
	(count 0))
    (dolist (thisl l)
      (setq total (+ total thisl)
	    count (1+ count)))
    (if (zerop count)
	0
      (/ total count))))

(defun timeclock-generate-report (&optional html-p)
  "Generate a summary report based on the current timelog file.
By default, the report is in plain text, but if the optional argument
HTML-P is non-nil, HTML markup is added."
  (interactive "P")
  (let ((log (timeclock-log-data))
	(today (timeclock-day-base)))
    (if html-p (insert "<p>"))
    (insert "Currently ")
    (let ((project (nth 2 timeclock-last-event))
	  (begin (nth 1 timeclock-last-event))
	  done)
      (if (timeclock-currently-in-p)
	  (insert "IN")
	(if (zerop (length project))
	    (progn (insert "Done Working Today")
		   (setq done t))
	  (insert "OUT")))
      (unless done
	(insert " since " (format-time-string "%Y/%m/%d %-I:%M %p" begin))
	(if html-p
	    (insert "<br>\n<b>")
	  (insert "\n*"))
	(if (timeclock-currently-in-p)
	    (insert "Working on "))
	(if html-p
	    (insert project "</b><br>\n")
	  (insert project "*\n"))
	(let ((proj-data (cdr (assoc project (timeclock-project-alist log))))
	      (two-weeks-ago (timeclock-seconds-to-time
			      (- (timeclock-time-to-seconds today)
				 (* 2 7 24 60 60))))
	      two-week-len today-len)
	  (while proj-data
	    (if (not (time-less-p
		      (timeclock-entry-begin (car proj-data)) today))
		(setq today-len (timeclock-entry-list-length proj-data)
		      proj-data nil)
	      (if (and (null two-week-len)
		       (not (time-less-p
			     (timeclock-entry-begin (car proj-data))
			     two-weeks-ago)))
		  (setq two-week-len (timeclock-entry-list-length proj-data)))
	      (setq proj-data (cdr proj-data))))
	  (if (null two-week-len)
	      (setq two-week-len today-len))
	  (if html-p (insert "<p>"))
	  (if today-len
	      (insert "\nTime spent on this task today: "
		      (timeclock-seconds-to-string today-len)
		      ".  In the last two weeks: "
		      (timeclock-seconds-to-string two-week-len))
	    (if two-week-len
		(insert "\nTime spent on this task in the last two weeks: "
			(timeclock-seconds-to-string two-week-len))))
	  (if html-p (insert "<br>"))
	  (insert "\n"
		  (timeclock-seconds-to-string (timeclock-workday-elapsed))
		  " worked today, "
		  (timeclock-seconds-to-string (timeclock-workday-remaining))
		  " remaining, done at "
		  (timeclock-when-to-leave-string) "\n")))
      (if html-p (insert "<p>"))
      (insert "\nThere have been "
	      (number-to-string
	       (length (timeclock-day-alist log)))
	      " days of activity, starting "
	      (caar (last (timeclock-day-alist log))))
      (if html-p (insert "</p>"))
      (when html-p
	(insert "<p>
<table>
<td width=\"25\"><br></td><td>
<table border=1 cellpadding=3>
<tr><th><i>Statistics</i></th>
    <th>Entire</th>
    <th>-30 days</th>
    <th>-3 mons</th>
    <th>-6 mons</th>
    <th>-1 year</th>
</tr>")
	(let* ((day-list (timeclock-day-list))
	       (thirty-days-ago (timeclock-seconds-to-time
				 (- (timeclock-time-to-seconds today)
				    (* 30 24 60 60))))
	       (three-months-ago (timeclock-seconds-to-time
				  (- (timeclock-time-to-seconds today)
				     (* 90 24 60 60))))
	       (six-months-ago (timeclock-seconds-to-time
				(- (timeclock-time-to-seconds today)
				   (* 180 24 60 60))))
	       (one-year-ago (timeclock-seconds-to-time
			      (- (timeclock-time-to-seconds today)
				 (* 365 24 60 60))))
	       (time-in  (vector (list t) (list t) (list t) (list t) (list t)))
	       (time-out (vector (list t) (list t) (list t) (list t) (list t)))
	       (breaks   (vector (list t) (list t) (list t) (list t) (list t)))
	       (workday  (vector (list t) (list t) (list t) (list t) (list t)))
	       (lengths  (vector '(0 0) thirty-days-ago three-months-ago
				 six-months-ago one-year-ago)))
	  ;; collect statistics from complete timelog
	  (dolist (day day-list)
	    (let ((i 0) (l 5))
	      (while (< i l)
		(unless (time-less-p
			 (timeclock-day-begin day)
			 (aref lengths i))
		  (let ((base (timeclock-time-to-seconds
			       (timeclock-day-base
				(timeclock-day-begin day)))))
		    (nconc (aref time-in i)
			   (list (- (timeclock-time-to-seconds
				     (timeclock-day-begin day))
				    base)))
		    (let ((span (timeclock-day-span day))
			  (len (timeclock-day-length day))
			  (req (timeclock-day-required day)))
		      ;; If the day's actual work length is less than
		      ;; 70% of its span, then likely the exit time
		      ;; and break amount are not worthwhile adding to
		      ;; the statistic
		      (when (and (> span 0)
				 (> (/ (float len) (float span)) 0.70))
			(nconc (aref time-out i)
			       (list (- (timeclock-time-to-seconds
					 (timeclock-day-end day))
					base)))
			(nconc (aref breaks i) (list (- span len))))
		      (if req
			  (setq len (+ len (- timeclock-workday req))))
		      (nconc (aref workday i) (list len)))))
		(setq i (1+ i)))))
	  ;; average statistics
	  (let ((i 0) (l 5))
	    (while (< i l)
	      (aset time-in i (timeclock-mean (cdr (aref time-in i))))
	      (aset time-out i (timeclock-mean (cdr (aref time-out i))))
	      (aset breaks i (timeclock-mean (cdr (aref breaks i))))
	      (aset workday i (timeclock-mean (cdr (aref workday i))))
	      (setq i (1+ i))))
	  ;; Output the HTML table
	  (insert "<tr>\n")
	  (insert "<td align=\"center\">Time in</td>\n")
	  (let ((i 0) (l 5))
	    (while (< i l)
	      (insert "<td align=\"right\">"
		      (timeclock-seconds-to-string (aref time-in i))
		      "</td>\n")
	      (setq i (1+ i))))
	  (insert "</tr>\n")

	  (insert "<tr>\n")
	  (insert "<td align=\"center\">Time out</td>\n")
	  (let ((i 0) (l 5))
	    (while (< i l)
	      (insert "<td align=\"right\">"
		      (timeclock-seconds-to-string (aref time-out i))
		      "</td>\n")
	      (setq i (1+ i))))
	  (insert "</tr>\n")

	  (insert "<tr>\n")
	  (insert "<td align=\"center\">Break</td>\n")
	  (let ((i 0) (l 5))
	    (while (< i l)
	      (insert "<td align=\"right\">"
		      (timeclock-seconds-to-string (aref breaks i))
		      "</td>\n")
	      (setq i (1+ i))))
	  (insert "</tr>\n")

	  (insert "<tr>\n")
	  (insert "<td align=\"center\">Workday</td>\n")
	  (let ((i 0) (l 5))
	    (while (< i l)
	      (insert "<td align=\"right\">"
		      (timeclock-seconds-to-string (aref workday i))
		      "</td>\n")
	      (setq i (1+ i))))
	  (insert "</tr>\n"))
	(insert "<tfoot>
<td colspan=\"6\" align=\"center\">
  <i>These are approximate figures</i></td>
</tfoot>
</table>
</td></table>")))))

;;; A helpful little function

(defun timeclock-visit-timelog ()
  "Open the file named by `timeclock-file' in another window."
  (interactive)
  (find-file-other-window timeclock-file))

(provide 'timeclock)

(run-hooks 'timeclock-load-hook)

;; make sure we know the list of reasons, projects, and have computed
;; the last event and current discrepancy.
(if (file-readable-p timeclock-file)
    (timeclock-reread-log))

;;; timeclock.el ends here
