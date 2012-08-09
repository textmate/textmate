;;; appt.el --- appointment notification functions

;; Copyright (C) 1989-1990, 1994, 1998, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Neil Mager <neilm@juliet.ll.mit.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Package: calendar

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

;;
;; appt.el - visible and/or audible notification of
;;           appointments from diary file.
;;
;;
;; Thanks to  Edward M. Reingold for much help and many suggestions,
;; And to many others for bug fixes and suggestions.
;;
;;
;; This functions in this file will alert the user of a
;; pending appointment based on his/her diary file.  This package
;; is documented in the Emacs manual.
;;
;; To activate this package, simply use (appt-activate 1).
;; A `diary-file' with appointments of the format described in the
;; documentation of the function `appt-check' is required.
;; Relevant customizable variables are also listed in the
;; documentation of that function.
;;
;; Today's appointment list is initialized from the diary when this
;; package is activated.  Additionally, the appointments list is
;; recreated automatically at 12:01am for those who do not logout
;; every day or are programming late.  It is also updated when the
;; `diary-file' (or a file it includes) is saved.  Calling
;; `appt-check' with an argument (or re-enabling the package) forces a
;; re-initialization at any time.
;;
;; In order to add or delete items from today's list, without
;; changing the diary file, use `appt-add' and `appt-delete'.
;;

;; Brief internal description - Skip this if you are not interested!
;;
;; The function `appt-make-list' creates the appointments list which
;; `appt-check' reads.
;;
;; You can change the way the appointment window is created/deleted by
;; setting the variables `appt-disp-window-function' and
;; `appt-delete-window-function'.  For instance, you could be set them
;; to functions that display appointments in pop-up frames, which are
;; lowered or iconified after `appt-display-interval' minutes.
;;

;;; Code:

(require 'diary-lib)


(defgroup appt nil
  "Appointment notification."
  :prefix "appt-"
  :group 'calendar)

(defcustom appt-message-warning-time 12
  "Default time in minutes before an appointment that the warning begins.
You probably want to make `appt-display-interval' a factor of this."
  :type 'integer
  :group 'appt)

(defcustom appt-warning-time-regexp "warntime \\([0-9]+\\)"
  "Regexp matching a string giving the warning time for an appointment.
The first subexpression matches the time in minutes (an integer).
This overrides the default `appt-message-warning-time'.
You may want to put this inside a diary comment (see `diary-comment-start').
For example, to be warned 30 minutes in advance of an appointment:
   2011/06/01 12:00 Do something ## warntime 30
"
  :version "24.1"
  :type 'regexp
  :group 'appt)

(defcustom appt-audible t
  "Non-nil means beep to indicate appointment."
  :type 'boolean
  :group 'appt)

;; TODO - add popup.
(defcustom appt-display-format 'window
  "How appointment reminders should be displayed.
The options are:
   window - use a separate window
   echo   - use the echo area
   nil    - no visible reminder.
See also `appt-audible' and `appt-display-mode-line'."
  :type '(choice
          (const :tag "Separate window" window)
          (const :tag "Echo-area" echo)
          (const :tag "No visible display" nil))
  :group 'appt
  :version "24.1") ; no longer inherit from deleted obsolete variables

(defcustom appt-display-mode-line t
  "Non-nil means display minutes to appointment and time on the mode line.
This is in addition to any other display of appointment messages.
The mode line updates every minute, independent of the value of
`appt-display-interval'."
  :type 'boolean
  :group 'appt)

(defcustom appt-display-duration 10
  "The number of seconds an appointment message is displayed.
Only relevant if reminders are to be displayed in their own window."
  :type 'integer
  :group 'appt)

(defcustom appt-display-diary t
  "Non-nil displays the diary when the appointment list is first initialized.
This occurs when this package is first activated, and then at
midnight when the appointment list updates."
  :type 'boolean
  :group 'appt)

(defcustom appt-display-interval 3
  "Interval in minutes at which to display appointment reminders.
Once an appointment becomes due, Emacs displays reminders every
`appt-display-interval' minutes.  You probably want to make
`appt-message-warning-time' be a multiple of this, so that you get
a final message displayed precisely when the appointment is due.

Note that this variable controls the interval at which
`appt-display-message' is called.  The mode line display (if active)
always updates every minute."
  :type 'integer
  :group 'appt)

(defcustom appt-disp-window-function 'appt-disp-window
  "Function called to display appointment window.
Only relevant if reminders are being displayed in a window.
It should take three string arguments: the number of minutes till
the appointment, the current time, and the text of the appointment.
Each argument may also be a list, if multiple appointments are
relevant at any one time."
  :type 'function
  :group 'appt)

(defcustom appt-delete-window-function 'appt-delete-window
  "Function called to remove appointment window and buffer.
Only relevant if reminders are being displayed in a window."
  :type 'function
  :group 'appt)


;;; Internal variables below this point.

(defconst appt-buffer-name "*appt-buf*"
  "Name of the appointments buffer.")

;; TODO Turn this into an alist?  It would be easier to add more
;; optional elements.
;; Why is the first element (MINUTES) rather than just MINUTES?
;; It may just inherit from diary-entries-list, where we have
;; ((MONTH DAY YEAR) ENTRY)
(defvar appt-time-msg-list nil
  "The list of appointments for today.
Use `appt-add' and `appt-delete' to add and delete appointments.
The original list is generated from today's `diary-entries-list', and
can be regenerated using the function `appt-check'.
Each element of the generated list has the form
\((MINUTES) STRING [FLAG] [WARNTIME])
where MINUTES is the time in minutes of the appointment after midnight,
and STRING is the description of the appointment.
FLAG and WARNTIME are not always present.  A non-nil FLAG
indicates that the element was made with `appt-add', so calling
`appt-make-list' again should preserve it.  If WARNTIME is non-nil,
it is an integer to use in place of `appt-message-warning-time'.")

(defconst appt-max-time (1- (* 24 60))
  "11:59pm in minutes - number of minutes in a day minus 1.")

(defvar appt-mode-string nil
  "String being displayed in the mode line saying you have an appointment.
The actual string includes the amount of time till the appointment.
Only used if `appt-display-mode-line' is non-nil.")
(put 'appt-mode-string 'risky-local-variable t) ; for 'face property

(defvar appt-prev-comp-time nil
  "Time of day (mins since midnight) at which we last checked appointments.")

(defvar appt-display-count 0
  "Internal variable used to count number of consecutive reminders.")

(defvar appt-timer nil
  "Timer used for diary appointment notifications (`appt-check').
If this is non-nil, appointment checking is active.")


;;; Functions.

(defun appt-display-message (string mins)
  "Display a reminder about an appointment.
The string STRING describes the appointment, due in integer MINS minutes.
The arguments may also be lists, where each element relates to a
separate appointment.  The variable `appt-display-format' controls
the format of the visible reminder.  If `appt-audible' is non-nil,
also calls `beep' for an audible reminder."
  (if appt-audible (beep 1))
  ;; Backwards compatibility: avoid passing lists to a-d-w-f if not necessary.
  (and (listp mins)
       (= (length mins) 1)
       (setq mins (car mins)
             string (car string)))
  (cond ((eq appt-display-format 'window)
         ;; TODO use calendar-month-abbrev-array rather than %b?
         (let ((time (format-time-string "%a %b %e " (current-time)))
               err)
           (condition-case err
               (funcall appt-disp-window-function
                        (if (listp mins)
                            (mapcar 'number-to-string mins)
                          (number-to-string mins))
                        time string)
             (wrong-type-argument
              (if (not (listp mins))
                  (signal (car err) (cdr err))
                (message "Argtype error in `appt-disp-window-function' - \
update it for multiple appts?")
                ;; Fallback to just displaying the first appt, as we used to.
                (funcall appt-disp-window-function
                         (number-to-string (car mins)) time
                         (car string))))))
         (run-at-time (format "%d sec" appt-display-duration)
                      nil
                      appt-delete-window-function))
        ((eq appt-display-format 'echo)
         (message "%s" (if (listp string)
                           (mapconcat 'identity string "\n")
                         string)))))

(defun appt-mode-line (min-to-app &optional abbrev)
  "Return an appointment string suitable for use in the mode-line.
MIN-TO-APP is a list of minutes, as strings.
If ABBREV is non-nil, abbreviates some text."
  ;; All this silliness is just to make the formatting slightly nicer.
  (let* ((multiple (> (length min-to-app) 1))
         (imin (if (or (not multiple)
                       (not (delete (car min-to-app) min-to-app)))
                   (car min-to-app))))
    (format "%s%s %s"
            (if abbrev "App't" "Appointment")
            (if multiple "s" "")
            (if (equal imin "0") "now"
              (format "in %s %s"
                      (or imin (mapconcat 'identity min-to-app ","))
                      (if abbrev "min."
                        (format "minute%s" (if (equal imin "1") "" "s"))))))))

(defun appt-check (&optional force)
  "Check for an appointment and update any reminder display.
If optional argument FORCE is non-nil, reparse the diary file for
appointments.  Otherwise the diary file is only parsed once per day,
or when it (or a file it includes) is saved.

Note: the time must be the first thing in the line in the diary
for a warning to be issued.  The format of the time can be either
24 hour or am/pm.  For example:

              02/23/89
                18:00 Dinner

              Thursday
                11:45am Lunch meeting.

Appointments are checked every `appt-display-interval' minutes.
The following variables control appointment notification:

`appt-display-format'
        Controls the format in which reminders are displayed.

`appt-audible'
        Non-nil means there is an audible component to reminders.

`appt-message-warning-time'
        The default number of minutes in advance at which reminders
        should start.

`appt-display-mode-line'
        Non-nil means show in the mode line a countdown to the
        time of each appointment, once reminders start.

`appt-display-interval'
        Interval in minutes at which to display appointment messages.

`appt-display-diary'
        Non-nil means display the diary whenever the appointment list is
        initialized (e.g. the first time we check for appointments each day).

The following variables are only relevant if reminders are being
displayed in a window:

`appt-display-duration'
        Number of seconds for which an appointment message is displayed.

`appt-disp-window-function'
        Function called to display appointment window.

`appt-delete-window-function'
        Function called to remove appointment window and buffer."
  (interactive "P")                     ; so people can force updates
  (let* ((prev-appt-mode-string appt-mode-string)
         (prev-appt-display-count appt-display-count)
         ;; Convert current time to minutes after midnight (12.01am = 1).
         (now (decode-time))
         (now-mins (+ (* 60 (nth 2 now)) (nth 1 now)))
         appt-mins appt-warn-time min-to-app min-list string-list)
    (save-excursion                   ; FIXME ?
      ;; At first check in any day, update appointments to today's list.
      (if (or force                      ; eg initialize, diary save
              (null appt-prev-comp-time) ; first check
              (< now-mins appt-prev-comp-time)) ; new day
          (ignore-errors
            (let ((diary-hook (if (assoc 'appt-make-list diary-hook)
                                  diary-hook
                                (cons 'appt-make-list diary-hook))))
              (if appt-display-diary
                  (diary)
                ;; Not displaying the diary, so we can ignore
                ;; diary-number-of-entries.  Since appt.el only
                ;; works on a daily basis, no need for more entries.
                (diary-list-entries (calendar-current-date) 1 t)))))
      ;; Reset everything now in case we somehow missed a minute,
      ;; or (more likely) an appt was deleted.  (This is the only
      ;; reason we need prev-appt-display-count.)
      (setq appt-prev-comp-time now-mins
            appt-mode-string nil
            appt-display-count 0)
      ;; If there are entries in the list get each time off of the
      ;; list and calculate the number of minutes until the appointment.
      ;; TODO we are looping over all the appointments each time.
      ;; We could instead sort them by the time at which we need to
      ;; start warning.  But then removing entries in the past becomes
      ;; less straightforward.
      (dolist (appt appt-time-msg-list)
        ;; Remove any entries that are in the past.
        ;; FIXME how can there be any such entries, given that this
        ;; function removes entries when they hit zero minutes,
        ;; and appt-make-list doesn't add any in the past in the first place?
        (if (< (setq appt-mins (caar appt)) now-mins)
            (setq appt-time-msg-list (cdr appt-time-msg-list))
          (setq appt-warn-time (or (nth 3 appt) appt-message-warning-time)
                min-to-app (- appt-mins now-mins))
          ;; If we have an appointment between midnight and
          ;; `appt-warn-time' minutes after midnight, we
          ;; must begin to issue a message before midnight.  Midnight
          ;; is considered 0 minutes and 11:59pm is 1439
          ;; minutes.  Therefore we must recalculate the minutes to
          ;; appointment variable.  It is equal to the number of
          ;; minutes before midnight plus the number of minutes after
          ;; midnight our appointment is.
          ;; FIXME but appt-make-list constructs appt-time-msg-list to only
          ;; contain entries with today's date, so this cannot work?
          ;; Also above we just removed anything with appt-mins < now-mins.
          (if (and (< appt-mins appt-warn-time)
                   (> (+ now-mins appt-warn-time) appt-max-time))
              (setq min-to-app (+ (- (1+ appt-max-time) now-mins)
                                  appt-mins)))
          ;; Issue warning if the appointment time is within the warning time.
          (when (and (<= min-to-app appt-warn-time)
                     (>= min-to-app 0))
            (push min-to-app min-list)
            (push (cadr appt) string-list)
            ;; When an appointment is reached, delete it from the list.
            (if (zerop min-to-app)
                (setq appt-time-msg-list (delete appt appt-time-msg-list))))))
      (when min-list
        (setq min-list (nreverse min-list)
              string-list (nreverse string-list))
        ;; This is true every appt-display-interval minutes from the
        ;; time at which we first started reminding.
        ;; TODO in the case of multiple appointments, whose interval
        ;; should we respect?  The first one that we started warning about?
        ;; That's what we do now, and this makes sense if you interpret
        ;; a-d-i as "don't remind me any more frequently than this".
        ;; But should we always show a message when a new appt becomes due?
        ;; When one appt gets removed, should we switch to the interval
        ;; of the next?
        (and (zerop (mod prev-appt-display-count appt-display-interval))
             (appt-display-message string-list min-list))
        (when appt-display-mode-line
          (setq appt-mode-string
                (concat " " (propertize
                             (appt-mode-line (mapcar 'number-to-string
                                                     min-list) t)
                             'face 'mode-line-emphasis))))
        ;; Reset count to 0 in case we display another appt on the next cycle.
        (setq appt-display-count (if (eq '(0) min-list) 0
                                   (1+ prev-appt-display-count))))
      ;; If we have changed the mode line string, redisplay all mode lines.
      (and appt-display-mode-line
           (not (string-equal appt-mode-string prev-appt-mode-string))
           (progn
             (force-mode-line-update t)
             ;; If the string now has a notification, redisplay right now.
             (if appt-mode-string
                 (sit-for 0)))))))

(defun appt-disp-window (min-to-app new-time appt-msg)
  "Display appointment due in MIN-TO-APP (a string) minutes.
NEW-TIME is a string giving the current date.
Displays the appointment message APPT-MSG in a separate buffer.
The arguments may also be lists, where each element relates to a
separate appointment."
  (let ((this-window (selected-window))
        (appt-disp-buf (get-buffer-create appt-buffer-name)))
    ;; Make sure we're not in the minibuffer before splitting the window.
    ;; FIXME this seems needlessly complicated?
    (when (minibufferp)
      (other-window 1)
      (and (minibufferp) (display-multi-frame-p) (other-frame 1)))
    (if (cdr (assq 'unsplittable (frame-parameters)))
        ;; In an unsplittable frame, use something somewhere else.
	(progn
	  (set-buffer appt-disp-buf)
	  (display-buffer appt-disp-buf))
      (unless (or (special-display-p (buffer-name appt-disp-buf))
                  (same-window-p (buffer-name appt-disp-buf)))
        ;; By default, split the bottom window and use the lower part.
        (appt-select-lowest-window)
        ;; Split the window, unless it's too small to do so.
        (when (>= (window-height) (* 2 window-min-height))
          (select-window (split-window))))
      (switch-to-buffer appt-disp-buf))
    (or (listp min-to-app)
        (setq min-to-app (list min-to-app)
              appt-msg (list appt-msg)))
    ;; I don't really see the point of the new-time argument.
    ;; It repeatedly reminds you of the date?
    ;; It would make more sense if it was eg the time of the appointment.
    ;; Let's allow it to be a list or not independent of the other elements.
    (or (listp new-time)
        (setq new-time (list new-time)))
    ;; FIXME Link to diary entry?
    (calendar-set-mode-line
     (format " %s. %s" (appt-mode-line min-to-app)
             (mapconcat 'identity new-time ", ")))
    (setq buffer-read-only nil
          buffer-undo-list t)
    (erase-buffer)
    ;; If we have appointments at different times, prepend the times.
    (if (or (= 1 (length min-to-app))
            (not (delete (car min-to-app) min-to-app)))
        (insert (mapconcat 'identity appt-msg "\n"))
      (dotimes (i (length appt-msg))
        (insert (format "%s%sm: %s" (if (> i 0) "\n" "")
                        (nth i min-to-app) (nth i appt-msg)))))
    (shrink-window-if-larger-than-buffer (get-buffer-window appt-disp-buf t))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (raise-frame (selected-frame))
    (select-window this-window)))

(defun appt-delete-window ()
  "Function called to undisplay appointment messages.
Usually just deletes the appointment buffer."
  (let ((window (get-buffer-window appt-buffer-name t)))
    (and window
         (or (eq window (frame-root-window (window-frame window)))
             (delete-window window))))
  (kill-buffer appt-buffer-name)
  (if appt-audible
      (beep 1)))

(defun appt-select-lowest-window ()
  "Select the lowest window on the frame."
  (let ((lowest-window (selected-window))
        (bottom-edge (nth 3 (window-edges)))
        next-bottom-edge)
    (walk-windows (lambda (w)
                    (when (< bottom-edge (setq next-bottom-edge
                                               (nth 3 (window-edges w))))
                      (setq bottom-edge next-bottom-edge
                            lowest-window w))) 'nomini)
    (select-window lowest-window)))

(defconst appt-time-regexp
  "[0-9]?[0-9]\\(h\\([0-9][0-9]\\)?\\|[:.][0-9][0-9]\\)\\(am\\|pm\\)?")

;;;###autoload
(defun appt-add (time msg &optional warntime)
  "Add an appointment for today at TIME with message MSG.
The time should be in either 24 hour format or am/pm format.
Optional argument WARNTIME is an integer (or string) giving the number
of minutes before the appointment at which to start warning.
The default is `appt-message-warning-time'."
  (interactive "sTime (hh:mm[am/pm]): \nsMessage: \n\
sMinutes before the appointment to start warning: ")
  (unless (string-match appt-time-regexp time)
    (error "Unacceptable time-string"))
  (and (stringp warntime)
       (setq warntime (unless (string-equal warntime "")
                        (string-to-number warntime))))
  (and warntime
       (not (integerp warntime))
       (error "Argument WARNTIME must be an integer, or nil"))
  (or appt-timer (appt-activate))
  (let ((time-msg (list (list (appt-convert-time time))
                        (concat time " " msg) t)))
    ;; It is presently nonsensical to have multiple warnings about
    ;; the same appointment with just different delays, but it might
    ;; not always be so.  TODO
    (if warntime (setq time-msg (append time-msg (list warntime))))
    (unless (member time-msg appt-time-msg-list)
      (setq appt-time-msg-list
            (appt-sort-list (nconc appt-time-msg-list (list time-msg)))))))

(defun appt-delete ()
  "Delete an appointment from the list of appointments."
  (interactive)
  (let ((tmp-msg-list appt-time-msg-list))
    (dolist (element tmp-msg-list)
      (if (y-or-n-p (concat "Delete "
                            ;; We want to quote any doublequotes in the
                            ;; string, as well as put doublequotes around it.
                            (prin1-to-string
                             (substring-no-properties (cadr element) 0))
                            " from list? "))
          (setq appt-time-msg-list (delq element appt-time-msg-list)))))
  (appt-check)
  (message ""))


(defvar number)
(defvar original-date)
(defvar diary-entries-list)

(defun appt-make-list ()
  "Update the appointments list from today's diary buffer.
The time must be at the beginning of a line for it to be
put in the appointments list (see examples in documentation of
the function `appt-check').  We assume that the variables DATE and
NUMBER hold the arguments that `diary-list-entries' received.
They specify the range of dates that the diary is being processed for.

Any appointments made with `appt-add' are not affected by this function."
  ;; We have something to do if the range of dates that the diary is
  ;; considering includes the current date.
  (if (and (not (calendar-date-compare
                 (list (calendar-current-date))
                 (list original-date)))
           (calendar-date-compare
            (list (calendar-current-date))
            (list (calendar-gregorian-from-absolute
                   (+ (calendar-absolute-from-gregorian original-date)
                      number)))))
      (save-excursion
        ;; Clear the appointments list, then fill it in from the diary.
        (dolist (elt appt-time-msg-list)
          ;; Delete any entries that were not made with appt-add.
          (unless (nth 2 elt)
            (setq appt-time-msg-list
                  (delq elt appt-time-msg-list))))
        (if diary-entries-list
            ;; Cycle through the entry-list (diary-entries-list)
            ;; looking for entries beginning with a time.  If the
            ;; entry begins with a time, add it to the
            ;; appt-time-msg-list.  Then sort the list.
            (let ((entry-list diary-entries-list)
                  time-string literal)
              ;; Below, we assume diary-entries-list was in date
              ;; order.  It is, unless something on
              ;; diary-list-entries-hook has changed it, eg
              ;; diary-include-other-files (bug#7019).  It must be
              ;; in date order if number = 1.
              (and diary-list-entries-hook
                   appt-display-diary
                   (not (eq diary-number-of-entries 1))
                   (not (memq (car (last diary-list-entries-hook))
                              '(diary-sort-entries sort-diary-entries)))
                   (setq entry-list (sort entry-list 'diary-entry-compare)))
              ;; Skip diary entries for dates before today.
              (while (and entry-list
                          (calendar-date-compare
                           (car entry-list) (list (calendar-current-date))))
                (setq entry-list (cdr entry-list)))
              ;; Parse the entries for today.
              (while (and entry-list
                          (calendar-date-equal
                           (calendar-current-date) (caar entry-list)))
                (setq time-string (cadr (car entry-list))
                      ;; Including any comments.
                      literal (or (nth 2 (nth 3 (car entry-list)))
                                  time-string))
                (while (string-match appt-time-regexp time-string)
                  (let* ((beg (match-beginning 0))
                         ;; Get just the time for this appointment.
                         (only-time (match-string 0 time-string))
                         ;; Find the end of this appointment
                         ;; (the start of the next).
                         (end (string-match
                               (concat "\n[ \t]*" appt-time-regexp)
                               time-string
                               (match-end 0)))
                         (warntime
                          (if (string-match appt-warning-time-regexp literal)
                              (string-to-number (match-string 1 literal))))
                         ;; Get the whole string for this appointment.
                         (appt-time-string
                          (substring time-string beg end))
                         ;; FIXME why the list?  It makes the first
                         ;; element (MINUTES) rather than MINUTES.
                         (appt-time (list (appt-convert-time only-time)))
                         (time-msg (append
                                    (list appt-time appt-time-string)
                                    (if warntime (list nil warntime)))))
                    ;; Add this appointment to appt-time-msg-list.
                    (setq appt-time-msg-list
                          (nconc appt-time-msg-list (list time-msg))
                          ;; Discard this appointment from the string.
                          ;; (This allows for multiple appts per entry.)
                          time-string
                          (if end (substring time-string end) ""))
                    ;; Similarly, discard the start of literal.
                    (and (> (length time-string) 0)
                         (string-match appt-time-regexp literal)
                         (setq end (string-match
                                    (concat "\n[ \t]*" appt-time-regexp)
                                    literal (match-end 0)))
                         (setq literal (substring literal end)))))
                (setq entry-list (cdr entry-list)))))
        (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))
        ;; Convert current time to minutes after midnight (12:01am = 1),
        ;; and remove elements in the list that are in the past.
        (let* ((now (decode-time))
               (now-mins (+ (* 60 (nth 2 now)) (nth 1 now))))
          (while (and appt-time-msg-list
                      (< (caar (car appt-time-msg-list)) now-mins))
            (setq appt-time-msg-list (cdr appt-time-msg-list)))))))


(defun appt-sort-list (appt-list)
  "Sort an appointment list, putting earlier items at the front.
APPT-LIST is a list of the same format as `appt-time-msg-list'."
  (sort appt-list (lambda (e1 e2) (< (caar e1) (caar e2)))))


(defun appt-convert-time (time2conv)
  "Convert hour:min[am/pm] format TIME2CONV to minutes from midnight.
A period (.) can be used instead of a colon (:) to separate the
hour and minute parts."
  ;; Formats that should be accepted:
  ;;   10:00 10.00 10h00 10h 10am 10:00am 10.00am
  (let ((min (if (string-match "[h:.]\\([0-9][0-9]\\)" time2conv)
                 (string-to-number (match-string 1 time2conv))
               0))
        (hr (if (string-match "[0-9]*[0-9]" time2conv)
                (string-to-number (match-string 0 time2conv))
              0)))
    ;; Convert the time appointment time into 24 hour time.
    (cond ((and (string-match "pm" time2conv) (< hr 12))
           (setq hr (+ 12 hr)))
          ((and (string-match "am" time2conv) (= hr 12))
           (setq hr 0)))
    ;; Convert the actual time into minutes.
    (+ (* hr 60) min)))

(defun appt-update-list ()
  "If the current buffer is visiting the diary, update appointments.
This function also acts on any file listed in `diary-included-files'.
It is intended for use with `write-file-functions'."
  (and (member buffer-file-name (append diary-included-files
                                        (list (expand-file-name diary-file))))
       appt-timer
       (let ((appt-display-diary nil))
         (appt-check t)))
  nil)

;;;###autoload
(defun appt-activate (&optional arg)
  "Toggle checking of appointments.
With optional numeric argument ARG, turn appointment checking on if
ARG is positive, otherwise off."
  (interactive "P")
  (let ((appt-active appt-timer))
    (setq appt-active (if arg (> (prefix-numeric-value arg) 0)
                        (not appt-active)))
    (remove-hook 'write-file-functions 'appt-update-list)
    (or global-mode-string (setq global-mode-string '("")))
    (delq 'appt-mode-string global-mode-string)
    (when appt-timer
      (cancel-timer appt-timer)
      (setq appt-timer nil))
    (if appt-active
        (progn
          (add-hook 'write-file-functions 'appt-update-list)
          (setq appt-timer (run-at-time t 60 'appt-check)
                global-mode-string
                (append global-mode-string '(appt-mode-string)))
          (appt-check t)
          (message "Appointment reminders enabled%s"
                   ;; Someone might want to use appt-add without a diary.
                   (if (ignore-errors (diary-check-diary-file))
                       ""
                     " (no diary file found)")))
      (message "Appointment reminders disabled"))))


(provide 'appt)

;;; appt.el ends here
