;;; type-break.el --- encourage rests from typing at appropriate intervals

;; Copyright (C) 1994-1995, 1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Noah Friedman
;; Maintainer: Noah Friedman <friedman@splode.com>
;; Keywords: extensions, timers
;; Created: 1994-07-13

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

;; The docstring for the function `type-break-mode' summarizes most of the
;; details of the interface.

;; This package relies on the assumption that you live entirely in Emacs,
;; as the author does.  If that's not the case for you (e.g. you often
;; suspend Emacs or work in other windows) then this won't help very much;
;; it will depend on just how often you switch back to Emacs.  At the very
;; least, you will want to turn off the keystroke thresholds and rest
;; interval tracking.

;; If you prefer not to be queried about taking breaks, but instead just
;; want to be reminded, do the following:
;;
;;   (setq type-break-query-mode nil)
;;
;; Or call the command `type-break-query-mode' with a negative prefix
;; argument.

;; If you find echo area messages annoying and would prefer to see messages
;; in the mode line instead, do M-x type-break-mode-line-message-mode
;; or set the variable of the same name to `t'.

;; This program can truly cons up a storm because of all the calls to
;; `current-time' (which always returns fresh conses).  I'm dismayed by
;; this, but I think the health of my hands is far more important than a
;; few pages of virtual memory.

;; This program has no hope of working in Emacs 18.

;; This package was inspired by Roland McGrath's hanoi-break.el.
;; Several people contributed feedback and ideas, including
;;      Roland McGrath <roland@gnu.org>
;;      Kleanthes Koniaris <kgk@koniaris.com>
;;      Mark Ashton <mpashton@gnu.org>
;;      Matt Wilding <wilding@cli.com>
;;      Robert S. Boyer <boyer@cs.utexas.edu>

;;; Code:


(defgroup type-break nil
  "Encourage the user to take a rest from typing at suitable intervals."
  :prefix "type-break"
  :group 'keyboard)

;;;###autoload
(defcustom type-break-mode nil
  "Toggle typing break mode.
See the docstring for the `type-break-mode' command for more information.
Setting this variable directly does not take effect;
use either \\[customize] or the function `type-break-mode'."
  :set (lambda (_symbol value)
	 (type-break-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'type-break
  :require 'type-break)

;;;###autoload
(defcustom type-break-interval (* 60 60)
  "Number of seconds between scheduled typing breaks."
  :type 'integer
  :group 'type-break)

;;;###autoload
(defcustom type-break-good-rest-interval (/ type-break-interval 6)
  "Number of seconds of idle time considered to be an adequate typing rest.

When this variable is non-nil, Emacs checks the idle time between
keystrokes.  If this idle time is long enough to be considered a \"good\"
rest from typing, then the next typing break is simply rescheduled for later.

If a break is interrupted before this much time elapses, the user will be
asked whether or not really to interrupt the break."
  :type 'integer
  :group 'type-break)

;;;###autoload
(defcustom type-break-good-break-interval nil
  "Number of seconds considered to be an adequate explicit typing rest.

When this variable is non-nil, its value is considered to be a \"good\"
length (in seconds) for a break initiated by the command `type-break',
overriding `type-break-good-rest-interval'.  This provides querying of
break interruptions when `type-break-good-rest-interval' is nil."
  :type 'integer
  :group 'type-break)

;;;###autoload
(defcustom type-break-keystroke-threshold
  ;; Assuming typing speed is 35wpm (on the average, do you really
  ;; type more than that in a minute?  I spend a lot of time reading mail
  ;; and simply studying code in buffers) and average word length is
  ;; about 5 letters, default upper threshold to the average number of
  ;; keystrokes one is likely to type in a break interval.  That way if the
  ;; user goes through a furious burst of typing activity, cause a typing
  ;; break to be required sooner than originally scheduled.
  ;; Conversely, the minimum threshold should be about a fifth of this.
  (let* ((wpm 35)
         (avg-word-length 5)
         (upper (* wpm avg-word-length (/ type-break-interval 60)))
         (lower (/ upper 5)))
    (cons lower upper))
  "Upper and lower bound on number of keystrokes for considering typing break.
This structure is a pair of numbers (MIN . MAX).

The first number is the minimum number of keystrokes that must have been
entered since the last typing break before considering another one, even if
the scheduled time has elapsed; the break is simply rescheduled until later
if the minimum threshold hasn't been reached.  If this first value is nil,
then there is no minimum threshold; as soon as the scheduled time has
elapsed, the user will always be queried.

The second number is the maximum number of keystrokes that can be entered
before a typing break is requested immediately, pre-empting the originally
scheduled break.  If this second value is nil, then no pre-emptive breaks
will occur; only scheduled ones will.

Keys with bucky bits (shift, control, meta, etc) are counted as only one
keystroke even though they really require multiple keys to generate them.

The command `type-break-guesstimate-keystroke-threshold' can be used to
guess a reasonably good pair of values for this variable."
  :type 'sexp
  :group 'type-break)

(defcustom type-break-query-function 'yes-or-no-p
  "Function to use for making query for a typing break.
It should take a string as an argument, the prompt.
Usually this should be set to `yes-or-no-p' or `y-or-n-p'.

To avoid being queried at all, set `type-break-query-mode' to nil."
  :type '(radio function
                (function-item yes-or-no-p)
                (function-item y-or-n-p))
  :group 'type-break)

(defcustom type-break-query-interval 60
  "Number of seconds between queries to take a break, if put off.
The user will continue to be prompted at this interval until he or she
finally submits to taking a typing break."
  :type 'integer
  :group 'type-break)

(defcustom type-break-time-warning-intervals '(300 120 60 30)
  "List of time intervals for warnings about upcoming typing break.
At each of the intervals (specified in seconds) away from a scheduled
typing break, print a warning in the echo area."
  :type '(repeat integer)
  :group 'type-break)

(defcustom type-break-keystroke-warning-intervals '(300 200 100 50)
  "List of keystroke measurements for warnings about upcoming typing break.
At each of the intervals (specified in keystrokes) away from the upper
keystroke threshold, print a warning in the echo area.
If either this variable or the upper threshold is set, then no warnings
will occur."
  :type '(repeat integer)
  :group 'type-break)

(defcustom type-break-warning-repeat 40
  "Number of keystrokes for which warnings should be repeated.
That is, for each of this many keystrokes the warning is redisplayed
in the echo area to make sure it's really seen."
  :type 'integer
  :group 'type-break)

(defcustom type-break-time-stamp-format "[%H:%M] "
  "Timestamp format used to prefix messages.
Format specifiers are as used by `format-time-string'."
  :type 'string
  :group 'type-break)

(defcustom type-break-demo-functions
  '(type-break-demo-boring type-break-demo-life type-break-demo-hanoi)
  "List of functions to consider running as demos during typing breaks.
When a typing break begins, one of these functions is selected randomly
to have Emacs do something interesting.

Any function in this list should start a demo which ceases as soon as a
key is pressed."
  :type '(repeat function)
  :group 'type-break)

(defcustom type-break-demo-boring-stats nil
  "Show word per minute and keystroke figures in the Boring demo."
  :type 'boolean
  :group 'type-break)

(defcustom type-break-terse-messages nil
  "Use slightly terser messages."
  :type 'boolean
  :group 'type-break)

(defcustom type-break-file-name (convert-standard-filename "~/.type-break")
  "Name of file used to save state across sessions.
If this is nil, no data will be saved across sessions."
  :type 'file
  :group 'type-break)

(defvar type-break-post-command-hook '(type-break-check)
  "Hook run indirectly by `post-command-hook' for typing break functions.
This is not really intended to be set by the user, but it's probably
harmless to do so.  Mainly it is used by various parts of the typing break
program to delay actions until after the user has completed some command.
It exists because `post-command-hook' itself is inaccessible while its
functions are being run, and some type-break--related functions want to
remove themselves after running.")


;; Mode line frobs

(defvar type-break-mode-line-format
  '(type-break-mode-line-message-mode
    (""
     type-break-mode-line-break-message
     type-break-mode-line-warning))
  "*Format of messages in the mode line concerning typing breaks.")

(defvar type-break-mode-line-break-message
  '(type-break-mode-line-break-message-p
    type-break-mode-line-break-string))

(defvar type-break-mode-line-break-message-p nil)
(defvar type-break-mode-line-break-string " *** TAKE A TYPING BREAK NOW ***")

(defvar type-break-mode-line-warning
      '(type-break-mode-line-break-message-p
        ("")
        (type-break-warning-countdown-string
         (" *** "
          "Break in "
          type-break-warning-countdown-string
          " "
          type-break-warning-countdown-string-type
          "***"))))

(defvar type-break-warning-countdown-string nil
  "If non-nil, this is a countdown for the next typing break.

This variable, in conjunction with `type-break-warning-countdown-string-type'
\(which indicates whether this value is a number of keystrokes or seconds)
is installed in `mode-line-format' to notify of imminent typing breaks.")

(defvar type-break-warning-countdown-string-type nil
  "Indicates the unit type of `type-break-warning-countdown-string'.
It will be either \"seconds\" or \"keystrokes\".")


;; These are internal variables.  Do not set them yourself.

(defvar type-break-alarm-p nil)
(defvar type-break-keystroke-count 0)
(defvar type-break-time-last-break nil)
(defvar type-break-time-next-break nil)
(defvar type-break-time-last-command (current-time))
(defvar type-break-current-time-warning-interval nil)
(defvar type-break-current-keystroke-warning-interval nil)
(defvar type-break-time-warning-count 0)
(defvar type-break-keystroke-warning-count 0)
(defvar type-break-interval-start nil)


;;;###autoload
(defun type-break-mode (&optional prefix)
  "Enable or disable typing-break mode.
This is a minor mode, but it is global to all buffers by default.

When this mode is enabled, the user is encouraged to take typing breaks at
appropriate intervals; either after a specified amount of time or when the
user has exceeded a keystroke threshold.  When the time arrives, the user
is asked to take a break.  If the user refuses at that time, Emacs will ask
again in a short period of time.  The idea is to give the user enough time
to find a good breaking point in his or her work, but be sufficiently
annoying to discourage putting typing breaks off indefinitely.

A negative prefix argument disables this mode.
No argument or any non-negative argument enables it.

The user may enable or disable this mode by setting the variable of the
same name, though setting it in that way doesn't reschedule a break or
reset the keystroke counter.

If the mode was previously disabled and is enabled as a consequence of
calling this function, it schedules a break with `type-break-schedule' to
make sure one occurs (the user can call that command to reschedule the
break at any time).  It also initializes the keystroke counter.

The variable `type-break-interval' specifies the number of seconds to
schedule between regular typing breaks.  This variable doesn't directly
affect the time schedule; it simply provides a default for the
`type-break-schedule' command.

If set, the variable `type-break-good-rest-interval' specifies the minimum
amount of time which is considered a reasonable typing break.  Whenever
that time has elapsed, typing breaks are automatically rescheduled for
later even if Emacs didn't prompt you to take one first.  Also, if a break
is ended before this much time has elapsed, the user will be asked whether
or not to continue.  A nil value for this variable prevents automatic
break rescheduling, making `type-break-interval' an upper bound on the time
between breaks.  In this case breaks will be prompted for as usual before
the upper bound if the keystroke threshold is reached.

If `type-break-good-rest-interval' is nil and
`type-break-good-break-interval' is set, then confirmation is required to
interrupt a break before `type-break-good-break-interval' seconds
have passed.  This provides for an upper bound on the time between breaks
together with confirmation of interruptions to these breaks.

The variable `type-break-keystroke-threshold' is used to determine the
thresholds at which typing breaks should be considered.  You can use
the command `type-break-guesstimate-keystroke-threshold' to try to
approximate good values for this.

There are several variables that affect how or when warning messages about
imminent typing breaks are displayed.  They include:

        `type-break-mode-line-message-mode'
        `type-break-time-warning-intervals'
        `type-break-keystroke-warning-intervals'
        `type-break-warning-repeat'
        `type-break-warning-countdown-string'
        `type-break-warning-countdown-string-type'

There are several variables that affect if, how, and when queries to begin
a typing break occur.  They include:

        `type-break-query-mode'
        `type-break-query-function'
        `type-break-query-interval'

The command `type-break-statistics' prints interesting things.

Finally, a file (named `type-break-file-name') is used to store information
across Emacs sessions.  This provides recovery of the break status between
sessions and after a crash.  Manual changes to the file may result in
problems."
  (interactive "P")
  (type-break-check-post-command-hook)

  (let ((already-enabled type-break-mode))
    (setq type-break-mode (>= (prefix-numeric-value prefix) 0))

    (cond
     ((and already-enabled type-break-mode)
      (and (called-interactively-p 'interactive)
           (message "Type Break mode is already enabled")))
     (type-break-mode
      (when type-break-file-name
	(with-current-buffer (find-file-noselect type-break-file-name 'nowarn)
	  (setq buffer-save-without-query t)))

      (or global-mode-string
          (setq global-mode-string '("")))
      (or (assq 'type-break-mode-line-message-mode
		minor-mode-alist)
	  (setq minor-mode-alist
		(cons type-break-mode-line-format
		      minor-mode-alist)))
      (type-break-keystroke-reset)
      (type-break-mode-line-countdown-or-break nil)

      (setq type-break-time-last-break
            (or (type-break-get-previous-time)
                (current-time)))

      ;; schedule according to break time from session file
      (type-break-schedule
       (let (diff)
         (if (and type-break-time-last-break
                  (< (setq diff (type-break-time-difference
                                 type-break-time-last-break
                                 (current-time)))
                     type-break-interval))
             ;; use the file's value
             (progn
               (setq type-break-keystroke-count
                     (type-break-get-previous-count))
               ;; file the time, in case it was read from the auto-save file
               (type-break-file-time type-break-interval-start)
               (setq type-break-interval-start type-break-time-last-break)
               (- type-break-interval diff))
           ;; schedule from now
           (setq type-break-interval-start (current-time))
           (type-break-file-time type-break-interval-start)
           type-break-interval))
       type-break-interval-start
       type-break-interval)

      (and (called-interactively-p 'interactive)
           (message "Type Break mode is enabled and set")))
     (t
      (type-break-keystroke-reset)
      (type-break-mode-line-countdown-or-break nil)
      (type-break-cancel-schedule)
      (do-auto-save)
      (when type-break-file-name
	(with-current-buffer (find-file-noselect type-break-file-name
						 'nowarn)
	  (set-buffer-modified-p nil)
	  (unlock-buffer)
	  (kill-this-buffer)))
      (and (called-interactively-p 'interactive)
           (message "Type Break mode is disabled")))))
  type-break-mode)

(define-minor-mode type-break-mode-line-message-mode
  "Toggle warnings about typing breaks in the mode line.
With a prefix argument ARG, enable these warnings if ARG is
positive, and disable them otherwise.  If called from Lisp,
enable them if ARG is omitted or nil.

The user may also enable or disable this mode simply by setting
the variable of the same name.

Variables controlling the display of messages in the mode line include:

        `mode-line-format'
        `global-mode-string'
        `type-break-mode-line-break-message'
        `type-break-mode-line-warning'"
  :global t)

(define-minor-mode type-break-query-mode
  "Toggle typing break queries.
With a prefix argument ARG, enable these queries if ARG is
positive, and disable them otherwise.  If called from Lisp,
enable them if ARG is omitted or nil.

The user may also enable or disable this mode simply by setting
the variable of the same name."
  :global t)


;;; session file functions

(defvar type-break-auto-save-file-name
  (let ((buffer-file-name type-break-file-name))
    (make-auto-save-file-name))
  "Auto-save name of `type-break-file-name'.")

(defun type-break-file-time (&optional time)
  "File break time in `type-break-file-name', unless the file is locked."
  (if (and type-break-file-name
           (not (stringp (file-locked-p type-break-file-name))))
      (with-current-buffer (find-file-noselect type-break-file-name
                                               'nowarn)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%s\n\n" (or time type-break-interval-start)))
          ;; file saving is left to auto-save
          ))))

(defun type-break-file-keystroke-count ()
  "File keystroke count in `type-break-file-name', unless the file is locked."
  (if (and type-break-file-name
           (not (stringp (file-locked-p type-break-file-name))))
      ;; Prevent deactivation of the mark in some other buffer.
      (let (deactivate-mark)
	(with-current-buffer (find-file-noselect type-break-file-name
						 'nowarn)
	  (save-excursion
	    (let ((inhibit-read-only t))
	      (goto-char (point-min))
	      (forward-line)
	      (delete-region (point) (line-end-position))
	      (insert (format "%s" type-break-keystroke-count))
	      ;; file saving is left to auto-save
	      ))))))

(defun timep (time)
  "If TIME is in the format returned by `current-time' then
return TIME, else return nil."
  (condition-case nil
      (and (float-time time) time)
    (error nil)))

(defun type-break-choose-file ()
  "Return file to read from."
  (cond
   ((not type-break-file-name)
    nil)
   ((and (file-exists-p type-break-auto-save-file-name)
         (file-readable-p type-break-auto-save-file-name))
    type-break-auto-save-file-name)
   ((and (file-exists-p type-break-file-name)
         (file-readable-p type-break-file-name))
    type-break-file-name)
   (t nil)))

(defun type-break-get-previous-time ()
  "Get previous break time from `type-break-file-name'.
Returns nil if the file is missing or if the time breaks with the
`current-time' format."
  (let ((file (type-break-choose-file)))
    (if file
        (timep ;; returns expected format, else nil
         (with-current-buffer (find-file-noselect file 'nowarn)
	   (condition-case nil
	       (save-excursion
		 (goto-char (point-min))
		 (read (current-buffer)))
	     (end-of-file
	      (error "End of file in `%s'" file))))))))

(defun type-break-get-previous-count ()
  "Get previous keystroke count from `type-break-file-name'.
Return 0 if the file is missing or if the form read is not an
integer."
  (let ((file (type-break-choose-file)))
    (if (and file
             (integerp
              (setq file
                    (with-current-buffer
                        (find-file-noselect file 'nowarn)
                    (condition-case nil
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line 1)
                          (read (current-buffer)))
                      (end-of-file
                       (error "End of file in `%s'" file)))))))
        file
      0)))


;;;###autoload
(defun type-break ()
  "Take a typing break.

During the break, a demo selected from the functions listed in
`type-break-demo-functions' is run.

After the typing break is finished, the next break is scheduled
as per the function `type-break-schedule'."
  (interactive)
  (do-auto-save)
  (type-break-cancel-schedule)
  ;; remove any query scheduled during interactive invocation
  (remove-hook 'type-break-post-command-hook 'type-break-do-query)
  (let ((continue t)
        (start-time (current-time)))
    (setq type-break-time-last-break start-time)
    (while continue
      (save-window-excursion
        ;; Eat the screen.
        (and (eq (selected-window) (minibuffer-window))
             (other-window 1))
        (delete-other-windows)
        (scroll-right (window-width))
        (unless type-break-terse-messages
          (message "Press any key to resume from typing break."))

        (random t)
        (let* ((len (length type-break-demo-functions))
               (idx (random len))
               (fn (nth idx type-break-demo-functions)))
          (condition-case ()
              (funcall fn)
            (error nil))))

      (let ((good-interval (or type-break-good-rest-interval
                               type-break-good-break-interval)))
        (cond
         (good-interval
          (let ((break-secs (type-break-time-difference
                             start-time (current-time))))
            (cond
             ((>= break-secs good-interval)
              (setq continue nil))
             ;; 60 seconds may be too much leeway if the break is only 3
             ;; minutes to begin with.  You can just say "no" to the query
             ;; below if you're in that much of a hurry.
             ;;((> 60 (abs (- break-secs good-interval)))
             ;; (setq continue nil))
             ((funcall
               type-break-query-function
               (format
                (if type-break-terse-messages
                    "%s%s remaining.  Continue break? "
                  "%sYou really ought to rest %s more.  Continue break? ")
                (type-break-time-stamp)
                (type-break-format-time (- good-interval
                                           break-secs)))))
             (t
              (setq continue nil)))))
         (t (setq continue nil))))))

  (type-break-keystroke-reset)
  (type-break-file-time)
  (type-break-mode-line-countdown-or-break nil)
  (type-break-schedule))


(defun type-break-schedule (&optional time start interval)
  "Schedule a typing break for TIME seconds from now.
If time is not specified it defaults to `type-break-interval'.
START and INTERVAL are used when recovering a break.
START is the start of the break (defaults to now).
INTERVAL is the full length of an interval (defaults to TIME)."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (or time (setq time type-break-interval))
  (type-break-check-post-command-hook)
  (type-break-cancel-schedule)
  (type-break-time-warning-schedule time 'reset)
  (type-break-run-at-time (max 1 time) nil 'type-break-alarm)
  (setq type-break-time-next-break
        (type-break-time-sum (or start (current-time))
                             (or interval time))))

(defun type-break-cancel-schedule ()
  (type-break-cancel-time-warning-schedule)
  (type-break-cancel-function-timers 'type-break-alarm)
  (setq type-break-alarm-p nil)
  (setq type-break-time-next-break nil))

(defun type-break-time-warning-schedule (&optional time resetp)
  (let ((type-break-current-time-warning-interval nil))
    (type-break-cancel-time-warning-schedule))
  (add-hook 'type-break-post-command-hook 'type-break-time-warning 'append)
  (cond
   (type-break-time-warning-intervals
    (and resetp
         (setq type-break-current-time-warning-interval
               type-break-time-warning-intervals))

    (or time
        (setq time (type-break-time-difference (current-time)
                                               type-break-time-next-break)))

    (while (and type-break-current-time-warning-interval
                (> (car type-break-current-time-warning-interval) time))
      (setq type-break-current-time-warning-interval
            (cdr type-break-current-time-warning-interval)))

    (cond
     (type-break-current-time-warning-interval
      (setq time (- time (car type-break-current-time-warning-interval)))
      (setq type-break-current-time-warning-interval
            (cdr type-break-current-time-warning-interval))

      ;(let (type-break-current-time-warning-interval)
      ;  (type-break-cancel-time-warning-schedule))
      (type-break-run-at-time (max 1 time) nil 'type-break-time-warning-alarm)

      (cond
       (resetp
        (setq type-break-warning-countdown-string nil))
       (t
        (setq type-break-warning-countdown-string (number-to-string time))
        (setq type-break-warning-countdown-string-type "seconds"))))))))

(defun type-break-cancel-time-warning-schedule ()
  (type-break-cancel-function-timers 'type-break-time-warning-alarm)
  (remove-hook 'type-break-post-command-hook 'type-break-time-warning)
  (setq type-break-current-time-warning-interval
        type-break-time-warning-intervals)
  (setq type-break-time-warning-count 0) ; avoid warnings after break
  (setq type-break-warning-countdown-string nil))

(defun type-break-alarm ()
  (type-break-check-post-command-hook)
  (setq type-break-alarm-p t)
  (type-break-mode-line-countdown-or-break 'break))

(defun type-break-time-warning-alarm ()
  (type-break-check-post-command-hook)
  (type-break-time-warning-schedule)
  (setq type-break-time-warning-count type-break-warning-repeat)
  (type-break-time-warning)
  (type-break-mode-line-countdown-or-break 'countdown))


(defun type-break-run-tb-post-command-hook ()
  (and type-break-mode
       (run-hooks 'type-break-post-command-hook)))

(defun type-break-check ()
  "Ask to take a typing break if appropriate.
This may be the case either because the scheduled time has come \(and the
minimum keystroke threshold has been reached\) or because the maximum
keystroke threshold has been exceeded."
  (type-break-file-keystroke-count)
  (let* ((min-threshold (car type-break-keystroke-threshold))
         (max-threshold (cdr type-break-keystroke-threshold)))
    (and type-break-good-rest-interval
         (progn
           (and (> (type-break-time-difference
                    type-break-time-last-command (current-time))
                   type-break-good-rest-interval)
                (progn
                  (type-break-keystroke-reset)
                  (type-break-mode-line-countdown-or-break nil)
                  (setq type-break-time-last-break (current-time))
                  (type-break-schedule)))
           (setq type-break-time-last-command (current-time))))

    (and type-break-keystroke-threshold
         (let ((keys (this-command-keys)))
           (cond
            ;; Ignore mouse motion
            ((and (vectorp keys)
                  (consp (aref keys 0))
                  (memq (car (aref keys 0)) '(mouse-movement))))
            (t
             (setq type-break-keystroke-count
                   (+ type-break-keystroke-count (length keys)))))))

    (cond
     (type-break-alarm-p
      (cond
       ((input-pending-p))
       ((eq (selected-window) (minibuffer-window)))
       ((and min-threshold
             (< type-break-keystroke-count min-threshold))
        (type-break-schedule))
       (t
        ;; If keystroke count is within min-threshold of
        ;; max-threshold, lower it to reduce the likelihood of an
        ;; immediate subsequent query.
        (and max-threshold
             min-threshold
             (< (- max-threshold type-break-keystroke-count) min-threshold)
             (progn
               (type-break-keystroke-reset)
               (setq type-break-keystroke-count min-threshold)))
        (type-break-query))))
     ((and type-break-keystroke-warning-intervals
           max-threshold
           (= type-break-keystroke-warning-count 0)
           (type-break-check-keystroke-warning)))
     ((and max-threshold
           (> type-break-keystroke-count max-threshold)
           (not (input-pending-p))
           (not (eq (selected-window) (minibuffer-window))))
      (type-break-keystroke-reset)
      (setq type-break-keystroke-count (or min-threshold 0))
      (type-break-query)))))

;; This should return t if warnings were enabled, nil otherwise.
(defun type-break-check-keystroke-warning ()
  ;; This is safe because the caller should have checked that the cdr was
  ;; non-nil already.
  (let ((left (- (cdr type-break-keystroke-threshold)
                 type-break-keystroke-count)))
    (cond
     ((null (car type-break-current-keystroke-warning-interval))
      nil)
     ((> left (car type-break-current-keystroke-warning-interval))
      nil)
     (t
      (while (and (car type-break-current-keystroke-warning-interval)
                  (< left (car type-break-current-keystroke-warning-interval)))
        (setq type-break-current-keystroke-warning-interval
              (cdr type-break-current-keystroke-warning-interval)))
      (setq type-break-keystroke-warning-count type-break-warning-repeat)
      (add-hook 'type-break-post-command-hook 'type-break-keystroke-warning)
      (setq type-break-warning-countdown-string (number-to-string left))
      (setq type-break-warning-countdown-string-type "keystrokes")
      (type-break-mode-line-countdown-or-break 'countdown)
      t))))

;; Arrange for a break query to be made, when the user stops typing furiously.
(defun type-break-query ()
  (add-hook 'type-break-post-command-hook 'type-break-do-query))

(defun type-break-do-query ()
  (cond
   ((not type-break-query-mode)
    (type-break-noninteractive-query)
    (type-break-schedule type-break-query-interval)
    (remove-hook 'type-break-post-command-hook 'type-break-do-query))
   ((sit-for 2)
    (condition-case ()
        (cond
         ((let ((type-break-mode nil)
                ;; yes-or-no-p sets this-command to exit-minibuffer,
                ;; which hoses undo or yank-pop (if you happened to be
                ;; yanking just when the query occurred).
                (this-command this-command))
            ;; Cancel schedule to prevent possibility of a second query
            ;; from taking place before this one has even returned.
            ;; The condition-case wrapper will reschedule on quit.
            (type-break-cancel-schedule)
            ;; Also prevent a second query when the break is interrupted.
            (remove-hook 'type-break-post-command-hook 'type-break-do-query)
            (funcall type-break-query-function
                     (format "%s%s"
                             (type-break-time-stamp)
			     (if type-break-terse-messages
				 "Break now? "
			       "Take a break from typing now? "))))
          (type-break))
         (t
          (type-break-schedule type-break-query-interval)))
      (quit
       (type-break-schedule type-break-query-interval))))))

(defun type-break-noninteractive-query (&optional _ignored-args)
  "Null query function which doesn't interrupt user and assumes `no'.
It prints a reminder in the echo area to take a break, but doesn't enforce
this or ask the user to start one right now."
  (cond
   (type-break-mode-line-message-mode)
   (t
    (beep t)
    (message "%sYou should take a typing break now.  Do `M-x type-break'."
             (type-break-time-stamp))
    (sit-for 1)
    (beep t)
    ;; return nil so query caller knows to reset reminder, as if user
    ;; said "no" in response to yes-or-no-p.
    nil)))

(defun type-break-time-warning ()
  (cond
   ((and (car type-break-keystroke-threshold)
         (< type-break-keystroke-count (car type-break-keystroke-threshold))))
   ((> type-break-time-warning-count 0)
    (let ((timeleft (type-break-time-difference (current-time)
                                                type-break-time-next-break)))
      (setq type-break-warning-countdown-string (number-to-string timeleft))
      (cond
       ((eq (selected-window) (minibuffer-window)))
       ;; Do nothing if the command was just a prefix arg, since that will
       ;; immediately be followed by some other interactive command.
       ;; Otherwise, it is particularly annoying for the sit-for below to
       ;; delay redisplay when one types sequences like `C-u -1 C-l'.
       ((memq this-command '(digit-argument universal-argument)))
       ((not type-break-mode-line-message-mode)
        ;; Pause for a moment so any previous message can be seen.
        (sit-for 2)
        (message "%sWarning: typing break due in %s."
                 (type-break-time-stamp)
                 (type-break-format-time timeleft))
        (setq type-break-time-warning-count
              (1- type-break-time-warning-count))))))
   (t
    (remove-hook 'type-break-post-command-hook 'type-break-time-warning)
    (setq type-break-warning-countdown-string nil))))

(defun type-break-keystroke-warning ()
  (cond
   ((> type-break-keystroke-warning-count 0)
    (setq type-break-warning-countdown-string
          (number-to-string (- (cdr type-break-keystroke-threshold)
                               type-break-keystroke-count)))
    (cond
     ((eq (selected-window) (minibuffer-window)))
     ;; Do nothing if the command was just a prefix arg, since that will
     ;; immediately be followed by some other interactive command.
     ;; Otherwise, it is particularly annoying for the sit-for below to
     ;; delay redisplay when one types sequences like `C-u -1 C-l'.
     ((memq this-command '(digit-argument universal-argument)))
     ((not type-break-mode-line-message-mode)
      (sit-for 2)
      (message "%sWarning: typing break due in %s keystrokes."
               (type-break-time-stamp)
               (- (cdr type-break-keystroke-threshold)
                  type-break-keystroke-count))
      (setq type-break-keystroke-warning-count
            (1- type-break-keystroke-warning-count)))))
   (t
    (remove-hook 'type-break-post-command-hook
                 'type-break-keystroke-warning)
    (setq type-break-warning-countdown-string nil))))

(defun type-break-mode-line-countdown-or-break (&optional type)
  (cond
   ((not type-break-mode-line-message-mode))
   ((eq type 'countdown)
    ;(setq type-break-mode-line-break-message-p nil)
    (add-hook 'type-break-post-command-hook
              'type-break-force-mode-line-update 'append))
   ((eq type 'break)
    ;; Alternate
    (setq type-break-mode-line-break-message-p
          (not type-break-mode-line-break-message-p))
    (remove-hook 'type-break-post-command-hook
                 'type-break-force-mode-line-update))
   (t
    (setq type-break-mode-line-break-message-p nil)
    (setq type-break-warning-countdown-string nil)
    (remove-hook 'type-break-post-command-hook
                 'type-break-force-mode-line-update)))
  (type-break-force-mode-line-update))


;;;###autoload
(defun type-break-statistics ()
  "Print statistics about typing breaks in a temporary buffer.
This includes the last time a typing break was taken, when the next one is
scheduled, the keystroke thresholds and the current keystroke count, etc."
  (interactive)
  (with-output-to-temp-buffer "*Typing Break Statistics*"
    (princ (format "Typing break statistics\n-----------------------\n
Typing break mode is currently %s.
Interactive query for breaks is %s.
Warnings of imminent typing breaks in mode line is %s.

Last typing break ended     : %s
Next scheduled typing break : %s\n
Minimum keystroke threshold : %s
Maximum keystroke threshold : %s
Current keystroke count     : %s"
                   (if type-break-mode "enabled" "disabled")
                   (if type-break-query-mode "enabled" "disabled")
                   (if type-break-mode-line-message-mode "enabled" "disabled")
                   (if type-break-time-last-break
                       (current-time-string type-break-time-last-break)
                     "never")
                   (if (and type-break-mode type-break-time-next-break)
                       (format "%s\t(%s from now)"
                               (current-time-string type-break-time-next-break)
                               (type-break-format-time
                                (type-break-time-difference
                                (current-time)
                                type-break-time-next-break)))
                     "none scheduled")
                   (or (car type-break-keystroke-threshold) "none")
                   (or (cdr type-break-keystroke-threshold) "none")
                   type-break-keystroke-count))))

;;;###autoload
(defun type-break-guesstimate-keystroke-threshold (wpm &optional wordlen frac)
  "Guess values for the minimum/maximum keystroke threshold for typing breaks.

If called interactively, the user is prompted for their guess as to how
many words per minute they usually type.  This value should not be your
maximum WPM, but your average.  Of course, this is harder to gauge since it
can vary considerably depending on what you are doing.  For example, one
tends to type less when debugging a program as opposed to writing
documentation.  (Perhaps a separate program should be written to estimate
average typing speed.)

From that, this command sets the values in `type-break-keystroke-threshold'
based on a fairly simple algorithm involving assumptions about the average
length of words (5).  For the minimum threshold, it uses about a fifth of
the computed maximum threshold.

When called from Lisp programs, the optional args WORDLEN and FRAC can be
used to override the default assumption about average word length and the
fraction of the maximum threshold to which to set the minimum threshold.
FRAC should be the inverse of the fractional value; for example, a value of
2 would mean to use one half, a value of 4 would mean to use one quarter, etc."
  (interactive "NOn average, how many words per minute do you type? ")
  (let* ((upper (* wpm (or wordlen 5) (/ type-break-interval 60)))
         (lower (/ upper (or frac 5))))
    (or type-break-keystroke-threshold
        (setq type-break-keystroke-threshold (cons nil nil)))
    (setcar type-break-keystroke-threshold lower)
    (setcdr type-break-keystroke-threshold upper)
    (if (called-interactively-p 'interactive)
        (message "min threshold: %d\tmax threshold: %d" lower upper))
    type-break-keystroke-threshold))


;;; misc functions

;; Compute the difference, in seconds, between a and b, two structures
;; similar to those returned by `current-time'.
(defun type-break-time-difference (a b)
  (round (float-time (time-subtract b a))))

;; Return (in a new list the same in structure to that returned by
;; `current-time') the sum of the arguments.  Each argument may be a time
;; list or a single integer, a number of seconds.
;; This function keeps the high and low 16 bits of the seconds properly
;; balanced so that the lower value never exceeds 16 bits.  Otherwise, when
;; the result is passed to `current-time-string' it will toss some of the
;; "low" bits and format the time incorrectly.
(defun type-break-time-sum (&rest tmlist)
  (let ((sum '(0 0 0)))
    (dolist (tem tmlist sum)
      (setq sum (time-add sum (if (integerp tem)
				  (list (floor tem 65536) (mod tem 65536))
				tem))))))

(defun type-break-time-stamp (&optional when)
  (if (fboundp 'format-time-string)
      (format-time-string type-break-time-stamp-format when)
    ;; Emacs 19.28 and prior do not have format-time-string.
    ;; In that case, result is not customizable.  Upgrade today!
    (format "[%s] " (substring (current-time-string when) 11 16))))

(defun type-break-format-time (secs)
  (let ((mins (/ secs 60)))
    (cond
     ((= mins 1) (format "%d minute" mins))
     ((> mins 0) (format "%d minutes" mins))
     ((= secs 1) (format "%d second" secs))
     (t (format "%d seconds" secs)))))

(defun type-break-keystroke-reset ()
  (setq type-break-interval-start (current-time)) ; not a keystroke
  (setq type-break-keystroke-count 0)
  (setq type-break-keystroke-warning-count 0)
  (setq type-break-current-keystroke-warning-interval
        type-break-keystroke-warning-intervals)
  (remove-hook 'type-break-post-command-hook 'type-break-keystroke-warning))

(defun type-break-force-mode-line-update (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL, force redisplay of all mode-lines."
  (and all (with-current-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p)))

;; If an exception occurs in Emacs while running the post command hook, the
;; value of that hook is clobbered.  This is because the value of the
;; variable is temporarily set to nil while it's running to prevent
;; recursive application, but it also means an exception aborts the routine
;; of restoring it.  This function is called from the timers to restore it,
;; just in case.
(defun type-break-check-post-command-hook ()
  (add-hook 'post-command-hook 'type-break-run-tb-post-command-hook 'append))


;;; Timer wrapper functions
;;
;; These shield type-break from variations in the interval timer packages
;; for different versions of Emacs.

(defun type-break-run-at-time (time repeat function)
  (condition-case nil (or (require 'timer) (require 'itimer)) (error nil))
  (run-at-time time repeat function))

(defvar timer-dont-exit)
(defun type-break-cancel-function-timers (function)
  (let ((timer-dont-exit t))
    (cancel-function-timers function)))


;;; Demo wrappers

(defun type-break-catch-up-event ()
  ;; If the last input event is a down-event, read and discard the
  ;; corresponding up-event too, to avoid triggering another prompt.
  (and (eventp last-input-event)
       (memq 'down (event-modifiers last-input-event))
       (read-event)))

;; This is a wrapper around hanoi that calls it with an arg large enough to
;; make the largest discs possible that will fit in the window.
;; Also, clean up the *Hanoi* buffer after we're done.
(defun type-break-demo-hanoi ()
  "Take a hanoiing typing break."
  (and (get-buffer "*Hanoi*")
       (kill-buffer "*Hanoi*"))
  (condition-case ()
      (progn
        (hanoi (/ (window-width) 8))
        ;; Wait for user to come back.
        (read-event)
	(type-break-catch-up-event)
        (kill-buffer "*Hanoi*"))
    (quit
     (read-event)
     (type-break-catch-up-event)
     (and (get-buffer "*Hanoi*")
          (kill-buffer "*Hanoi*")))))

;; This is a wrapper around life that calls it with a `sleep' arg to make
;; it run a little more leisurely.
;; Also, clean up the *Life* buffer after we're done.
(defun type-break-demo-life ()
  "Take a typing break and get a life."
  (let ((continue t))
    (while continue
      (setq continue nil)
      (and (get-buffer "*Life*")
           (kill-buffer "*Life*"))
      (condition-case ()
          (progn
            (life 3)
            ;; wait for user to return
            (read-event)
	    (type-break-catch-up-event)
            (kill-buffer "*Life*"))
        (life-extinct
         (message "%s" (get 'life-extinct 'error-message))
         ;; restart demo
         (setq continue t))
        (quit
	 (type-break-catch-up-event)
         (and (get-buffer "*Life*")
              (kill-buffer "*Life*")))))))

;; Boring demo, but doesn't use many cycles
(defun type-break-demo-boring ()
  "Boring typing break demo."
  (let ((rmsg (if type-break-terse-messages
                  ""
                "Press any key to resume from typing break"))
        (buffer-name "*Typing Break Buffer*")
        lines elapsed timeleft tmsg)
    (condition-case ()
        (progn
          (switch-to-buffer (get-buffer-create buffer-name))
          (buffer-disable-undo (current-buffer))
          (setq lines (/ (window-body-height) 2))
          (unless type-break-terse-messages (setq lines (1- lines)))
          (if type-break-demo-boring-stats
              (setq lines (- lines 2)))
          (setq lines (make-string lines ?\C-j))
          (while (not (input-pending-p))
            (erase-buffer)
            (setq elapsed (type-break-time-difference
                           type-break-time-last-break
                           (current-time)))
            (let ((good-interval (or type-break-good-rest-interval
                                     type-break-good-break-interval)))
              (cond
               (good-interval
                (setq timeleft (- good-interval elapsed))
                (if (> timeleft 0)
                    (setq tmsg
                          (format (if type-break-terse-messages
                                      "Break remaining: %s"
                                    "You should rest for %s more")
                                  (type-break-format-time timeleft)))
                  (setq tmsg
                        (format (if type-break-terse-messages
                                    "Break complete (%s elapsed in total)"
                                  "Typing break has lasted %s")
                                (type-break-format-time elapsed)))))
               (t
                (setq tmsg
                      (format (if type-break-terse-messages
                                  "Break has lasted %s"
                                "Typing break has lasted %s")
                              (type-break-format-time elapsed))))))
            (insert lines
                    (make-string (/ (- (window-width) (length tmsg)) 2) ?\ )
                    tmsg)
            (if (> (length rmsg) 0)
                (insert "\n"
                        (make-string (/ (- (window-width) (length rmsg)) 2)
                                     ?\ )
                        rmsg))
            (if type-break-demo-boring-stats
                (let*
                    ((message
                      (format
                       (if type-break-terse-messages
                           "Since last break: %s keystrokes\n"
                         "Since your last break you've typed %s keystrokes\n")
                       type-break-keystroke-count))
                     (column-spaces
                      (make-string (/ (- (window-width) (length message)) 2)
                                   ?\ ))
                     (wpm (/ (/ (float type-break-keystroke-count) 5)
                             (/ (type-break-time-difference
                                 type-break-interval-start
                                 type-break-time-last-break)
                                60.0))))
                  (insert "\n\n" column-spaces message)
                  (if type-break-terse-messages
                      (insert (format "                  %s%.2f wpm"
                                      column-spaces
                                      wpm))
                    (setq message
                          (format "at an average of %.2f words per minute"
                                  wpm))
                    (insert
                     (make-string (/ (- (window-width) (length message)) 2)
                                  ?\ )
                     message))))
            (goto-char (point-min))
            (sit-for 60))
	  (read-event)
	  (type-break-catch-up-event)
          (kill-buffer buffer-name))
      (quit
       (and (get-buffer buffer-name)
            (kill-buffer buffer-name))))))


(provide 'type-break)

(if type-break-mode
    (type-break-mode 1))

;;; type-break.el ends here
