;;; org-clock.el --- The time clocking code for Org-mode

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the time clocking code for Org-mode

(require 'org)
(require 'org-exp)
;;; Code:

(eval-when-compile
  (require 'cl))

(declare-function calendar-absolute-from-iso "cal-iso" (&optional date))
(declare-function notifications-notify "notifications" (&rest params))
(declare-function org-pop-to-buffer-same-window "org-compat" (&optional buffer-or-name norecord label))
(defvar org-time-stamp-formats)
(defvar org-ts-what)

(defgroup org-clock nil
  "Options concerning clocking working time in Org-mode."
  :tag "Org Clock"
  :group 'org-progress)

(defcustom org-clock-into-drawer org-log-into-drawer
  "Should clocking info be wrapped into a drawer?
When t, clocking info will always be inserted into a :LOGBOOK: drawer.
If necessary, the drawer will be created.
When nil, the drawer will not be created, but used when present.
When an integer and the number of clocking entries in an item
reaches or exceeds this number, a drawer will be created.
When a string, it names the drawer to be used.

The default for this variable is the value of `org-log-into-drawer',
which see."
  :group 'org-todo
  :group 'org-clock
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Only when drawer exists" nil)
	  (integer :tag "When at least N clock entries")
	  (const :tag "Into LOGBOOK drawer" "LOGBOOK")
	  (string :tag "Into Drawer named...")))

(defun org-clock-into-drawer ()
  "Return the value of `org-clock-into-drawer', but let properties overrule.
If the current entry has or inherits a CLOCK_INTO_DRAWER
property, it will be used instead of the default value; otherwise
if the current entry has or inherits a LOG_INTO_DRAWER property,
it will be used instead of the default value.
The default is the value of the customizable variable `org-clock-into-drawer',
which see."
  (let ((p (org-entry-get nil "CLOCK_INTO_DRAWER" 'inherit))
	(q (org-entry-get nil "LOG_INTO_DRAWER" 'inherit)))
    (cond
     ((or (not (or p q)) (equal p "nil") (equal q "nil")) org-clock-into-drawer)
     ((or (equal p "t") (equal q "t")) "LOGBOOK")
     ((not p) q)
     (t p))))

(defcustom org-clock-out-when-done t
  "When non-nil, clock will be stopped when the clocked entry is marked DONE.
DONE here means any DONE-like state.
A nil value means clock will keep running until stopped explicitly with
`C-c C-x C-o', or until the clock is started in a different item.
Instead of t, this can also be a list of TODO states that should trigger
clocking out."
  :group 'org-clock
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes, when done" t)
	  (repeat :tag "State list"
		  (string :tag "TODO keyword"))))

(defcustom org-clock-out-remove-zero-time-clocks nil
  "Non-nil means remove the clock line when the resulting time is zero."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-in-switch-to-state nil
  "Set task to a special todo state while clocking it.
The value should be the state to which the entry should be
switched.  If the value is a function, it must take one
parameter (the current TODO state of the item) and return the
state to switch it to."
  :group 'org-clock
  :group 'org-todo
  :type '(choice
	  (const :tag "Don't force a state" nil)
	  (string :tag "State")
	  (symbol :tag "Function")))

(defcustom org-clock-out-switch-to-state nil
  "Set task to a special todo state after clocking out.
The value should be the state to which the entry should be
switched.  If the value is a function, it must take one
parameter (the current TODO state of the item) and return the
state to switch it to."
  :group 'org-clock
  :group 'org-todo
  :type '(choice
	  (const :tag "Don't force a state" nil)
	  (string :tag "State")
	  (symbol :tag "Function")))

(defcustom org-clock-history-length 5
  "Number of clock tasks to remember in history."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-goto-may-find-recent-task t
  "Non-nil means `org-clock-goto' can go to recent task if no active clock."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-heading-function nil
  "When non-nil, should be a function to create `org-clock-heading'.
This is the string shown in the mode line when a clock is running.
The function is called with point at the beginning of the headline."
  :group 'org-clock
  :type 'function)

(defcustom org-clock-string-limit 0
  "Maximum length of clock strings in the modeline.  0 means no limit."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-in-resume nil
  "If non-nil, resume clock when clocking into task with open clock.
When clocking into a task with a clock entry which has not been closed,
the clock can be resumed from that point."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-persist nil
  "When non-nil, save the running clock when Emacs is closed.
The clock is resumed when Emacs restarts.
When this is t, both the running clock, and the entire clock
history are saved.  When this is the symbol `clock', only the
running clock is saved.

When Emacs restarts with saved clock information, the file containing the
running clock as well as all files mentioned in the clock history will
be visited.
All this depends on running `org-clock-persistence-insinuate' in .emacs"
  :group 'org-clock
  :type '(choice
	  (const :tag "Just the running clock" clock)
	  (const :tag "Just the history" history)
	  (const :tag "Clock and history" t)
	  (const :tag "No persistence" nil)))

(defcustom org-clock-persist-file (convert-standard-filename
				   "~/.emacs.d/org-clock-save.el")
  "File to save clock data to."
  :group 'org-clock
  :type 'string)

(defcustom org-clock-persist-query-save nil
  "When non-nil, ask before saving the current clock on exit."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-persist-query-resume t
  "When non-nil, ask before resuming any stored clock during load."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-sound nil
  "Sound that will used for notifications.
Possible values:

nil        no sound played.
t          standard Emacs beep
file name  play this sound file.  If not possible, fall back to beep"
  :group 'org-clock
  :type '(choice
	  (const :tag "No sound" nil)
	  (const :tag "Standard beep" t)
	  (file :tag "Play sound file")))

(defcustom org-clock-modeline-total 'auto
  "Default setting for the time included for the modeline clock.
This can be overruled locally using the CLOCK_MODELINE_TOTAL property.
Allowed values are:

current  Only the time in the current instance of the clock
today    All time clocked into this task today
repeat   All time clocked into this task since last repeat
all      All time ever recorded for this task
auto     Automatically, either `all', or `repeat' for repeating tasks"
  :group 'org-clock
  :type '(choice
	  (const :tag "Current clock" current)
	  (const :tag "Today's task time" today)
	  (const :tag "Since last repeat" repeat)
	  (const :tag "All task time" all)
	  (const :tag "Automatically, `all' or since `repeat'" auto)))

(defvaralias 'org-task-overrun-text 'org-clock-task-overrun-text)
(defcustom org-clock-task-overrun-text nil
  "The extra modeline text that should indicate that the clock is overrun.
The can be nil to indicate that instead of adding text, the clock time
should get a different face (`org-mode-line-clock-overrun').
When this is a string, it is prepended to the clock string as an indication,
also using the face `org-mode-line-clock-overrun'."
  :group 'org-clock
  :version "24.1"
  :type '(choice
	  (const :tag "Just mark the time string" nil)
	  (string :tag "Text to prepend")))

(defcustom org-show-notification-handler nil
  "Function or program to send notification with.
The function or program will be called with the notification
string as argument."
  :group 'org-clock
  :type '(choice
	  (string :tag "Program")
	  (function :tag "Function")))

(defgroup org-clocktable nil
  "Options concerning the clock table in Org-mode."
  :tag "Org Clock Table"
  :group 'org-clock)

(defcustom org-clocktable-defaults
  `(list
    :maxlevel 2
    :lang ,org-export-default-language
    :scope 'file
    :block nil
    :tstart nil
    :tend nil
    :step nil
    :stepskip0 nil
    :fileskip0 nil
    :tags nil
    :emphasize nil
    :link nil
    :narrow '40!
    :indent t
    :formula nil
    :timestamp nil
    :level nil
    :tcolumns nil
    :formatter nil)
  "Default properties for clock tables."
  :group 'org-clock
  :version "24.1"
  :type 'plist)

(defcustom org-clock-clocktable-formatter 'org-clocktable-write-default
  "Function to turn clocking data into a table.
For more information, see `org-clocktable-write-default'."
  :group 'org-clocktable
  :version "24.1"
  :type 'function)

;; FIXME: translate es and nl last string "Clock summary at"
(defcustom org-clock-clocktable-language-setup
  '(("en" "File"     "L"  "Timestamp"  "Headline" "Time"  "ALL"   "Total time"   "File time" "Clock summary at")
    ("es" "Archivo"  "N"  "Fecha y hora" "Tarea" "Tiempo" "TODO" "Tiempo total" "Tiempo archivo" "Clock summary at")
    ("fr" "Fichier"  "N"  "Horodatage" "En-tête"  "Durée" "TOUT"  "Durée totale" "Durée fichier" "Horodatage sommaire à")
    ("nl" "Bestand"  "N"  "Tijdstip"   "Hoofding" "Duur"  "ALLES" "Totale duur"  "Bestandstijd" "Clock summary at"))
  "Terms used in clocktable, translated to different languages."
  :group 'org-clocktable
  :version "24.1"
  :type 'alist)

(defcustom org-clock-clocktable-default-properties '(:maxlevel 2 :scope file)
  "Default properties for new clocktables.
These will be inserted into the BEGIN line, to make it easy for users to
play with them."
  :group 'org-clocktable
  :type 'plist)

(defcustom org-clock-idle-time nil
  "When non-nil, resolve open clocks if the user is idle more than X minutes."
  :group 'org-clock
  :type '(choice
	  (const :tag "Never" nil)
	  (integer :tag "After N minutes")))

(defcustom org-clock-auto-clock-resolution 'when-no-clock-is-running
  "When to automatically resolve open clocks found in Org buffers."
  :group 'org-clock
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "When no clock is running" when-no-clock-is-running)))

(defcustom org-clock-report-include-clocking-task nil
  "When non-nil, include the current clocking task time in clock reports."
  :group 'org-clock
  :version "24.1"
  :type 'boolean)

(defcustom org-clock-resolve-expert nil
  "Non-nil means do not show the splash buffer with the clock resolver."
  :group 'org-clock
  :version "24.1"
  :type 'boolean)

(defvar org-clock-in-prepare-hook nil
  "Hook run when preparing the clock.
This hook is run before anything happens to the task that
you want to clock in.  For example, you can use this hook
to add an effort property.")
(defvar org-clock-in-hook nil
  "Hook run when starting the clock.")
(defvar org-clock-out-hook nil
  "Hook run when stopping the current clock.")

(defvar org-clock-cancel-hook nil
  "Hook run when cancelling the current clock.")
(defvar org-clock-goto-hook nil
  "Hook run when selecting the currently clocked-in entry.")
(defvar org-clock-has-been-used nil
  "Has the clock been used during the current Emacs session?")

;;; The clock for measuring work time.

(defvar org-mode-line-string "")
(put 'org-mode-line-string 'risky-local-variable t)

(defvar org-clock-mode-line-timer nil)
(defvar org-clock-idle-timer nil)
(defvar org-clock-heading) ; defined in org.el
(defvar org-clock-heading-for-remember "")
(defvar org-clock-start-time "")

(defvar org-clock-leftover-time nil
  "If non-nil, user cancelled a clock; this is when leftover time started.")

(defvar org-clock-effort ""
  "Effort estimate of the currently clocking task.")

(defvar org-clock-total-time nil
  "Holds total time, spent previously on currently clocked item.
This does not include the time in the currently running clock.")

(defvar org-clock-history nil
  "List of marker pointing to recent clocked tasks.")

(defvar org-clock-default-task (make-marker)
  "Marker pointing to the default task that should clock time.
The clock can be made to switch to this task after clocking out
of a different task.")

(defvar org-clock-interrupted-task (make-marker)
  "Marker pointing to the task that has been interrupted by the current clock.")

(defvar org-clock-mode-line-map (make-sparse-keymap))
(define-key org-clock-mode-line-map [mode-line mouse-2] 'org-clock-goto)
(define-key org-clock-mode-line-map [mode-line mouse-1] 'org-clock-menu)

(defun org-clock-menu ()
  (interactive)
  (popup-menu
   '("Clock"
     ["Clock out" org-clock-out t]
     ["Change effort estimate" org-clock-modify-effort-estimate t]
     ["Go to clock entry" org-clock-goto t]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"])))

(defun org-clock-history-push (&optional pos buffer)
  "Push a marker to the clock history."
  (setq org-clock-history-length (max 1 (min 35 org-clock-history-length)))
  (let ((m (move-marker (make-marker)
			(or pos (point)) (org-base-buffer
					  (or buffer (current-buffer)))))
	n l)
    (while (setq n (member m org-clock-history))
      (move-marker (car n) nil))
    (setq org-clock-history
	  (delq nil
		(mapcar (lambda (x) (if (marker-buffer x) x nil))
			org-clock-history)))
    (when (>= (setq l (length org-clock-history)) org-clock-history-length)
      (setq org-clock-history
	    (nreverse
	     (nthcdr (- l org-clock-history-length -1)
		     (nreverse org-clock-history)))))
    (push m org-clock-history)))

(defun org-clock-save-markers-for-cut-and-paste (beg end)
  "Save relative positions of markers in region."
  (org-check-and-save-marker org-clock-marker beg end)
  (org-check-and-save-marker org-clock-hd-marker beg end)
  (org-check-and-save-marker org-clock-default-task beg end)
  (org-check-and-save-marker org-clock-interrupted-task beg end)
  (mapc (lambda (m) (org-check-and-save-marker m beg end))
	org-clock-history))

(defun org-clocking-buffer ()
  "Return the clocking buffer if we are currently clocking a task or nil."
  (marker-buffer org-clock-marker))

(defun org-clocking-p ()
  "Return t when clocking a task."
  (not (equal (org-clocking-buffer) nil)))

(defvar org-clock-before-select-task-hook nil
  "Hook called in task selection just before prompting the user.")

(defun org-clock-select-task (&optional prompt)
  "Select a task that recently was associated with clocking."
  (interactive)
  (let (sel-list rpl (i 0) s)
    (save-window-excursion
      (org-switch-to-buffer-other-window
       (get-buffer-create "*Clock Task Select*"))
      (erase-buffer)
      (when (marker-buffer org-clock-default-task)
	(insert (org-add-props "Default Task\n" nil 'face 'bold))
	(setq s (org-clock-insert-selection-line ?d org-clock-default-task))
	(push s sel-list))
      (when (marker-buffer org-clock-interrupted-task)
	(insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
	(setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
	(push s sel-list))
      (when (org-clocking-p)
	(insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
	(setq s (org-clock-insert-selection-line ?c org-clock-marker))
	(push s sel-list))
      (insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
      (mapc
       (lambda (m)
	 (when (marker-buffer m)
	   (setq i (1+ i)
		 s (org-clock-insert-selection-line
		    (if (< i 10)
			(+ i ?0)
		      (+ i (- ?A 10))) m))
	   (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
	   (push s sel-list)))
       org-clock-history)
      (run-hooks 'org-clock-before-select-task-hook)
      (org-fit-window-to-buffer)
      (message (or prompt "Select task for clocking:"))
      (setq rpl (read-char-exclusive))
      (cond
       ((eq rpl ?q) nil)
       ((eq rpl ?x) nil)
       ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
       (t (error "Invalid task choice %c" rpl))))))

(defun org-clock-insert-selection-line (i marker)
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (when (marker-buffer marker)
    (let (file cat task heading prefix)
      (with-current-buffer (org-base-buffer (marker-buffer marker))
	(save-excursion
	  (save-restriction
	    (widen)
	    (ignore-errors
	      (goto-char marker)
	      (setq file (buffer-file-name (marker-buffer marker))
		    cat (org-get-category)
		    heading (org-get-heading 'notags)
		    prefix (save-excursion
			     (org-back-to-heading t)
			     (looking-at org-outline-regexp)
			     (match-string 0))
		    task (substring
			  (org-fontify-like-in-org-mode
			   (concat prefix heading)
			   org-odd-levels-only)
			  (length prefix)))))))
      (when (and cat task)
	(insert (format "[%c] %-15s %s\n" i cat task))
	(cons i marker)))))

(defvar org-clock-task-overrun nil
  "Internal flag indicating if the clock has overrun the planned time.")
(defvar org-clock-update-period 60
  "Number of seconds between mode line clock string updates.")

(defun org-clock-get-clock-string ()
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
  (let* ((clocked-time (org-clock-get-clocked-time))
	 (h (floor clocked-time 60))
	 (m (- clocked-time (* 60 h))))
    (if org-clock-effort
	(let* ((effort-in-minutes
		(org-duration-string-to-minutes org-clock-effort))
	       (effort-h (floor effort-in-minutes 60))
	       (effort-m (- effort-in-minutes (* effort-h 60)))
	       (work-done-str
		(org-propertize
		 (format org-time-clocksum-format h m)
		 'face (if (and org-clock-task-overrun (not org-clock-task-overrun-text))
			   'org-mode-line-clock-overrun 'org-mode-line-clock)))
	       (effort-str (format org-time-clocksum-format effort-h effort-m))
	       (clockstr (org-propertize
			  (concat  "[%s/" effort-str
				   "] (" (replace-regexp-in-string "%" "%%" org-clock-heading) ")")
			  'face 'org-mode-line-clock)))
	  (format clockstr work-done-str))
      (org-propertize (format
		       (concat "[" org-time-clocksum-format " (%s)]")
		       h m org-clock-heading)
		      'face 'org-mode-line-clock))))

(defun org-clock-update-mode-line ()
  (if org-clock-effort
      (org-clock-notify-once-if-expired)
    (setq org-clock-task-overrun nil))
  (setq org-mode-line-string
	(org-propertize
	 (let ((clock-string (org-clock-get-clock-string))
	       (help-text "Org-mode clock is running.\nmouse-1 shows a menu\nmouse-2 will jump to task"))
	   (if (and (> org-clock-string-limit 0)
		    (> (length clock-string) org-clock-string-limit))
	       (org-propertize
		(substring clock-string 0 org-clock-string-limit)
		'help-echo (concat help-text ": " org-clock-heading))
	     (org-propertize clock-string 'help-echo help-text)))
	 'local-map org-clock-mode-line-map
	 'mouse-face (if (featurep 'xemacs) 'highlight 'mode-line-highlight)
	 ))
  (if (and org-clock-task-overrun org-clock-task-overrun-text)
      (setq org-mode-line-string
	    (concat (org-propertize
		     org-clock-task-overrun-text
		     'face 'org-mode-line-clock-overrun) org-mode-line-string)))
  (force-mode-line-update))

(defun org-clock-get-clocked-time ()
  "Get the clocked time for the current item in minutes.
The time returned includes the time spent on this task in
previous clocking intervals."
  (let ((currently-clocked-time
	 (floor (- (org-float-time)
		   (org-float-time org-clock-start-time)) 60)))
    (+ currently-clocked-time (or org-clock-total-time 0))))

(defun org-clock-modify-effort-estimate (&optional value)
 "Add to or set the effort estimate of the item currently being clocked.
VALUE can be a number of minutes, or a string with format hh:mm or mm.
When the string starts with a + or a - sign, the current value of the effort
property will be changed by that amount.
This will update the \"Effort\" property of currently clocked item, and
the mode line."
 (interactive)
 (when (org-clock-is-active)
   (let ((current org-clock-effort) sign)
     (unless value
       ;; Prompt user for a value or a change
       (setq value
	     (read-string
	      (format "Set effort (hh:mm or mm%s): "
		      (if current
			  (format ", prefix + to add to %s" org-clock-effort)
			"")))))
     (when (stringp value)
       ;; A string.  See if it is a delta
       (setq sign (string-to-char value))
       (if (member sign '(?- ?+))
	   (setq current (org-duration-string-to-minutes current)
		 value (substring value 1))
	 (setq current 0))
       (setq value (org-duration-string-to-minutes value))
       (if (equal ?- sign)
	   (setq value (- current value))
	 (if (equal ?+ sign) (setq value (+ current value)))))
     (setq value (max 0 value)
	   org-clock-effort (org-minutes-to-hh:mm-string value))
     (org-entry-put org-clock-marker "Effort" org-clock-effort)
     (org-clock-update-mode-line)
     (message "Effort is now %s" org-clock-effort))))

(defvar org-clock-notification-was-shown nil
  "Shows if we have shown notification already.")

(defun org-clock-notify-once-if-expired ()
  "Show notification if we spent more time than we estimated before.
Notification is shown only once."
  (when (org-clocking-p)
    (let ((effort-in-minutes (org-duration-string-to-minutes org-clock-effort))
	  (clocked-time (org-clock-get-clocked-time)))
      (if (setq org-clock-task-overrun
		(if (or (null effort-in-minutes) (zerop effort-in-minutes))
		    nil
		  (>= clocked-time effort-in-minutes)))
	  (unless org-clock-notification-was-shown
	    (setq org-clock-notification-was-shown t)
	    (org-notify
	     (format "Task '%s' should be finished by now. (%s)"
		     org-clock-heading org-clock-effort) t))
	(setq org-clock-notification-was-shown nil)))))

(defun org-notify (notification &optional play-sound)
  "Send a NOTIFICATION and maybe PLAY-SOUND."
  (org-show-notification notification)
  (if play-sound (org-clock-play-sound)))

(defun org-show-notification (notification)
  "Show notification.
Use `org-show-notification-handler' if defined,
use libnotify if available, or fall back on a message."
  (cond ((functionp org-show-notification-handler)
	 (funcall org-show-notification-handler notification))
	((stringp org-show-notification-handler)
	 (start-process "emacs-timer-notification" nil
			org-show-notification-handler notification))
	((featurep 'notifications)
	 (require 'notifications)
	 (notifications-notify
	  :title "Org-mode message"
	  :body notification
	  ;; FIXME how to link to the Org icon?
	  ;; :app-icon "~/.emacs.d/icons/mail.png"
	  :urgency 'low))
	((org-program-exists "notify-send")
	 (start-process "emacs-timer-notification" nil
			"notify-send" notification))
	;; Maybe the handler will send a message, so only use message as
	;; a fall back option
	(t (message "%s" notification))))

(defun org-clock-play-sound ()
  "Play sound as configured by `org-clock-sound'.
Use alsa's aplay tool if available."
  (cond
   ((not org-clock-sound))
   ((eq org-clock-sound t) (beep t) (beep t))
   ((stringp org-clock-sound)
    (let ((file (expand-file-name org-clock-sound)))
      (if (file-exists-p file)
	  (if (org-program-exists "aplay")
	      (start-process "org-clock-play-notification" nil
			     "aplay" file)
	    (condition-case nil
		(play-sound-file file)
	      (error (beep t) (beep t)))))))))

(defun org-program-exists (program-name)
  "Checks whenever we can locate PROGRAM-NAME using the `which' executable."
  (if (member system-type '(gnu/linux darwin))
      (= 0 (call-process "which" nil nil nil program-name))))

(defvar org-clock-mode-line-entry nil
  "Information for the modeline about the running clock.")

(defun org-find-open-clocks (file)
  "Search through the given file and find all open clocks."
  (let ((buf (or (get-file-buffer file)
		 (find-file-noselect file)))
	clocks)
    (with-current-buffer buf
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "CLOCK: \\(\\[.*?\\]\\)$" nil t)
	  (push (cons (copy-marker (match-end 1) t)
		      (org-time-string-to-time (match-string 1))) clocks))))
    clocks))

(defsubst org-is-active-clock (clock)
  "Return t if CLOCK is the currently active clock."
  (and (org-clock-is-active)
       (= org-clock-marker (car clock))))

(defmacro org-with-clock-position (clock &rest forms)
  "Evaluate FORMS with CLOCK as the current active clock."
  `(with-current-buffer (marker-buffer (car ,clock))
     (save-excursion
       (save-restriction
	 (widen)
	 (goto-char (car ,clock))
	 (beginning-of-line)
	 ,@forms))))
(def-edebug-spec org-with-clock-position (form body))
(put 'org-with-clock-position 'lisp-indent-function 1)

(defmacro org-with-clock (clock &rest forms)
  "Evaluate FORMS with CLOCK as the current active clock.
This macro also protects the current active clock from being altered."
  `(org-with-clock-position ,clock
     (let ((org-clock-start-time (cdr ,clock))
	   (org-clock-total-time)
	   (org-clock-history)
	   (org-clock-effort)
	   (org-clock-marker (car ,clock))
	   (org-clock-hd-marker (save-excursion
				  (outline-back-to-heading t)
				  (point-marker))))
       ,@forms)))
(def-edebug-spec org-with-clock (form body))
(put 'org-with-clock 'lisp-indent-function 1)

(defsubst org-clock-clock-in (clock &optional resume start-time)
  "Clock in to the clock located by CLOCK.
If necessary, clock-out of the currently active clock."
  (org-with-clock-position clock
    (let ((org-clock-in-resume (or resume org-clock-in-resume)))
      (org-clock-in nil start-time))))

(defsubst org-clock-clock-out (clock &optional fail-quietly at-time)
  "Clock out of the clock located by CLOCK."
  (let ((temp (copy-marker (car clock)
			   (marker-insertion-type (car clock)))))
    (if (org-is-active-clock clock)
	(org-clock-out fail-quietly at-time)
      (org-with-clock clock
	(org-clock-out fail-quietly at-time)))
    (setcar clock temp)))

(defsubst org-clock-clock-cancel (clock)
  "Cancel the clock located by CLOCK."
  (let ((temp (copy-marker (car clock)
			   (marker-insertion-type (car clock)))))
    (if (org-is-active-clock clock)
	(org-clock-cancel)
      (org-with-clock clock
	(org-clock-cancel)))
    (setcar clock temp)))

(defvar org-clock-clocking-in nil)
(defvar org-clock-resolving-clocks nil)
(defvar org-clock-resolving-clocks-due-to-idleness nil)

(defun org-clock-resolve-clock (clock resolve-to clock-out-time
				      &optional close-p restart-p fail-quietly)
  "Resolve `CLOCK' given the time `RESOLVE-TO', and the present.
`CLOCK' is a cons cell of the form (MARKER START-TIME)."
  (let ((org-clock-resolving-clocks t))
    (cond
     ((null resolve-to)
      (org-clock-clock-cancel clock)
      (if (and restart-p (not org-clock-clocking-in))
	  (org-clock-clock-in clock)))

     ((eq resolve-to 'now)
      (if restart-p
	  (error "RESTART-P is not valid here"))
      (if (or close-p org-clock-clocking-in)
	  (org-clock-clock-out clock fail-quietly)
	(unless (org-is-active-clock clock)
	  (org-clock-clock-in clock t))))

     ((not (time-less-p resolve-to (current-time)))
      (error "RESOLVE-TO must refer to a time in the past"))

     (t
      (if restart-p
	  (error "RESTART-P is not valid here"))
      (org-clock-clock-out clock fail-quietly (or clock-out-time
						  resolve-to))
      (unless org-clock-clocking-in
	(if close-p
	    (setq org-clock-leftover-time (and (null clock-out-time)
					       resolve-to))
	  (org-clock-clock-in clock nil (and clock-out-time
					     resolve-to))))))))

(defun org-clock-jump-to-current-clock (&optional effective-clock)
  (interactive)
  (let ((org-clock-into-drawer (org-clock-into-drawer))
	(clock (or effective-clock (cons org-clock-marker
					 org-clock-start-time))))
    (unless (marker-buffer (car clock))
      (error "No clock is currently running"))
    (org-with-clock clock (org-clock-goto))
    (with-current-buffer (marker-buffer (car clock))
      (goto-char (car clock))
      (if org-clock-into-drawer
	  (let ((logbook
		 (if (stringp org-clock-into-drawer)
		     (concat ":" org-clock-into-drawer ":")
		   ":LOGBOOK:")))
	    (ignore-errors
	      (outline-flag-region
	       (save-excursion
		 (outline-back-to-heading t)
		 (search-forward logbook)
		 (goto-char (match-beginning 0)))
	       (save-excursion
		 (outline-back-to-heading t)
		 (search-forward logbook)
		 (search-forward ":END:")
		 (goto-char (match-end 0)))
	       nil)))))))

(defun org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
  "Resolve an open org-mode clock.
An open clock was found, with `dangling' possibly being non-nil.
If this function was invoked with a prefix argument, non-dangling
open clocks are ignored.  The given clock requires some sort of
user intervention to resolve it, either because a clock was left
dangling or due to an idle timeout.  The clock resolution can
either be:

  (a) deleted, the user doesn't care about the clock
  (b) restarted from the current time (if no other clock is open)
  (c) closed, giving the clock X minutes
  (d) closed and then restarted
  (e) resumed, as if the user had never left

The format of clock is (CONS MARKER START-TIME), where MARKER
identifies the buffer and position the clock is open at (and
thus, the heading it's under), and START-TIME is when the clock
was started."
  (assert clock)
  (let* ((ch
	  (save-window-excursion
	    (save-excursion
	      (unless org-clock-resolving-clocks-due-to-idleness
		(org-clock-jump-to-current-clock clock))
	      (unless org-clock-resolve-expert
		(with-output-to-temp-buffer "*Org Clock*"
		  (princ "Select a Clock Resolution Command:

i/q/C-g  Ignore this question; the same as keeping all the idle time.

k/K      Keep X minutes of the idle time (default is all).  If this
         amount is less than the default, you will be clocked out
         that many minutes after the time that idling began, and then
         clocked back in at the present time.
g/G      Indicate that you \"got back\" X minutes ago.  This is quite
         different from 'k': it clocks you out from the beginning of
         the idle period and clock you back in X minutes ago.
s/S      Subtract the idle time from the current clock.  This is the
         same as keeping 0 minutes.
C        Cancel the open timer altogether.  It will be as though you
         never clocked in.
j/J      Jump to the current clock, to make manual adjustments.

For all these options, using uppercase makes your final state
to be CLOCKED OUT.")))
	      (org-fit-window-to-buffer (get-buffer-window "*Org Clock*"))
	      (let (char-pressed)
		(when (featurep 'xemacs)
		  (message (concat (funcall prompt-fn clock)
				   " [jkKgGsScCiq]? "))
		  (setq char-pressed (read-char-exclusive)))
		(while (or (null char-pressed)
			   (and (not (memq char-pressed
					   '(?k ?K ?g ?G ?s ?S ?C
						?j ?J ?i ?q)))
				(or (ding) t)))
		  (setq char-pressed
			(read-char (concat (funcall prompt-fn clock)
					   " [jkKgGSscCiq]? ")
				   nil 45)))
		(and (not (memq char-pressed '(?i ?q))) char-pressed)))))
	 (default
	   (floor (/ (org-float-time
		      (time-subtract (current-time) last-valid)) 60)))
	 (keep
	  (and (memq ch '(?k ?K))
	       (read-number "Keep how many minutes? " default)))
	 (gotback
	  (and (memq ch '(?g ?G))
	       (read-number "Got back how many minutes ago? " default)))
	 (subtractp (memq ch '(?s ?S)))
	 (barely-started-p (< (- (org-float-time last-valid)
				 (org-float-time (cdr clock))) 45))
	 (start-over (and subtractp barely-started-p)))
    (cond
     ((memq ch '(?j ?J))
      (if (eq ch ?J)
	  (org-clock-resolve-clock clock 'now nil t nil fail-quietly))
      (org-clock-jump-to-current-clock clock))
     ((or (null ch)
	  (not (memq ch '(?k ?K ?g ?G ?s ?S ?C))))
      (message ""))
     (t
      (org-clock-resolve-clock
       clock (cond
	      ((or (eq ch ?C)
		   ;; If the time on the clock was less than a minute before
		   ;; the user went away, and they've ask to subtract all the
		   ;; time...
		   start-over)
	       nil)
	      ((or subtractp
		   (and gotback (= gotback 0)))
	       last-valid)
	      ((or (and keep (= keep default))
		   (and gotback (= gotback default)))
	       'now)
	      (keep
	       (time-add last-valid (seconds-to-time (* 60 keep))))
	      (gotback
	       (time-subtract (current-time)
			      (seconds-to-time (* 60 gotback))))
	      (t
	       (error "Unexpected, please report this as a bug")))
       (and gotback last-valid)
       (memq ch '(?K ?G ?S))
       (and start-over
	    (not (memq ch '(?K ?G ?S ?C))))
       fail-quietly)))))

(defun org-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
  "Resolve all currently open org-mode clocks.
If `only-dangling-p' is non-nil, only ask to resolve dangling
\(i.e., not currently open and valid) clocks."
  (interactive "P")
  (unless org-clock-resolving-clocks
    (let ((org-clock-resolving-clocks t))
      (dolist (file (org-files-list))
	(let ((clocks (org-find-open-clocks file)))
	  (dolist (clock clocks)
	    (let ((dangling (or (not (org-clock-is-active))
				(/= (car clock) org-clock-marker))))
	      (if (or (not only-dangling-p) dangling)
		(org-clock-resolve
		 clock
		 (or prompt-fn
		     (function
		      (lambda (clock)
			(format
			 "Dangling clock started %d mins ago"
			 (floor
			  (/ (- (org-float-time (current-time))
				(org-float-time (cdr clock))) 60))))))
		 (or last-valid
		     (cdr clock)))))))))))

(defun org-emacs-idle-seconds ()
  "Return the current Emacs idle time in seconds, or nil if not idle."
  (let ((idle-time (current-idle-time)))
    (if idle-time
	(org-float-time idle-time)
      0)))

(defun org-mac-idle-seconds ()
  "Return the current Mac idle time in seconds."
  (string-to-number (shell-command-to-string "ioreg -c IOHIDSystem | perl -ane 'if (/Idle/) {$idle=(pop @F)/1000000000; print $idle; last}'")))

(defun org-x11-idle-seconds ()
  "Return the current X11 idle time in seconds."
  (/ (string-to-number (shell-command-to-string "x11idle")) 1000))

(defun org-user-idle-seconds ()
  "Return the number of seconds the user has been idle for.
This routine returns a floating point number."
  (cond
   ((eq system-type 'darwin)
    (org-mac-idle-seconds))
   ((eq window-system 'x)
    (org-x11-idle-seconds))
   (t
    (org-emacs-idle-seconds))))

(defvar org-clock-user-idle-seconds)

(defun org-resolve-clocks-if-idle ()
  "Resolve all currently open org-mode clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  (when (and org-clock-idle-time (not org-clock-resolving-clocks)
	     org-clock-marker)
    (let* ((org-clock-user-idle-seconds (org-user-idle-seconds))
	   (org-clock-user-idle-start
	    (time-subtract (current-time)
			   (seconds-to-time org-clock-user-idle-seconds)))
	   (org-clock-resolving-clocks-due-to-idleness t))
      (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
	  (org-clock-resolve
	   (cons org-clock-marker
		 org-clock-start-time)
	   (function
	    (lambda (clock)
	      (format "Clocked in & idle for %.1f mins"
		      (/ (org-float-time
			  (time-subtract (current-time)
					 org-clock-user-idle-start))
			 60.0))))
	   org-clock-user-idle-start)))))

(defvar org-clock-current-task nil
  "Task currently clocked in.")
(defun org-clock-set-current ()
  "Set `org-clock-current-task' to the task currently clocked in."
  (setq org-clock-current-task (nth 4 (org-heading-components))))

(defun org-clock-delete-current ()
  "Reset `org-clock-current-task' to nil."
  (setq org-clock-current-task nil))

(defun org-clock-in (&optional select start-time)
  "Start the clock on the current item.
If necessary, clock-out of the currently active clock.
With a prefix argument SELECT (\\[universal-argument]), offer a list of \
recently clocked tasks to
clock into.  When SELECT is \\[universal-argument] \\[universal-argument], \
clock into the current task and mark
is as the default task, a special task that will always be offered in
the clocking selection, associated with the letter `d'."
  (interactive "P")
  (setq org-clock-notification-was-shown nil)
  (catch 'abort
    (let ((interrupting (and (not org-clock-resolving-clocks-due-to-idleness)
			     (org-clocking-p)))
	  ts selected-task target-pos (msg-extra "")
	  (leftover (and (not org-clock-resolving-clocks)
			  org-clock-leftover-time)))

      (when (and org-clock-auto-clock-resolution
		 (or (not interrupting)
		     (eq t org-clock-auto-clock-resolution))
		 (not org-clock-clocking-in)
		 (not org-clock-resolving-clocks))
	(setq org-clock-leftover-time nil)
	(let ((org-clock-clocking-in t))
	  (org-resolve-clocks)))	; check if any clocks are dangling

      (when (equal select '(4))
	(setq selected-task (org-clock-select-task "Clock-in on task: "))
	(if selected-task
	    (setq selected-task (copy-marker selected-task))
	  (error "Abort")))

      (when (equal select '(16))
	;; Mark as default clocking task
	(org-clock-mark-default-task))

      (when interrupting
	;; We are interrupting the clocking of a different task.
	;; Save a marker to this task, so that we can go back.
	;; First check if we are trying to clock into the same task!
	(when (save-excursion
		(unless selected-task
		  (org-back-to-heading t))
		(and (equal (marker-buffer org-clock-hd-marker)
			    (if selected-task
				(marker-buffer selected-task)
			      (current-buffer)))
		     (= (marker-position org-clock-hd-marker)
			(if selected-task
			    (marker-position selected-task)
			  (point)))
		     (equal org-clock-current-task (nth 4 (org-heading-components)))))
	  (message "Clock continues in \"%s\"" org-clock-heading)
	  (throw 'abort nil))
	(move-marker org-clock-interrupted-task
		     (marker-position org-clock-marker)
		     (marker-buffer org-clock-marker))
	(let ((org-clock-clocking-in t))
	  (org-clock-out t)))

      ;; Clock in at which position?
      (setq target-pos
	    (if (and (eobp) (not (org-at-heading-p)))
		(point-at-bol 0)
	      (point)))
      (run-hooks 'org-clock-in-prepare-hook)
      (save-excursion
	(when (and selected-task (marker-buffer selected-task))
	  ;; There is a selected task, move to the correct buffer
	  ;; and set the new target position.
	  (set-buffer (org-base-buffer (marker-buffer selected-task)))
	  (setq target-pos (marker-position selected-task))
	  (move-marker selected-task nil))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char target-pos)
	    (org-back-to-heading t)
	    (or interrupting (move-marker org-clock-interrupted-task nil))
	    (org-clock-history-push)
	    (org-clock-set-current)
	    (cond ((functionp org-clock-in-switch-to-state)
		   (looking-at org-complex-heading-regexp)
		   (let ((newstate (funcall org-clock-in-switch-to-state
					    (match-string 2))))
		     (if newstate (org-todo newstate))))
		  ((and org-clock-in-switch-to-state
			(not (looking-at (concat org-outline-regexp "[ \t]*"
						 org-clock-in-switch-to-state
						 "\\>"))))
		   (org-todo org-clock-in-switch-to-state)))
	    (setq org-clock-heading-for-remember
		  (and (looking-at org-complex-heading-regexp)
		       (match-end 4)
		       (org-trim (buffer-substring (match-end 1)
						   (match-end 4)))))
	    (setq org-clock-heading
		  (cond ((and org-clock-heading-function
			      (functionp org-clock-heading-function))
			 (funcall org-clock-heading-function))
			((looking-at org-complex-heading-regexp)
			 (replace-regexp-in-string
			  "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
			  (match-string 4)))
			(t "???")))
	    (setq org-clock-heading (org-propertize org-clock-heading
						    'face nil))
	    (org-clock-find-position org-clock-in-resume)
	    (cond
	     ((and org-clock-in-resume
		   (looking-at
		    (concat "^[ \t]*" org-clock-string
			    " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
			    " *\\sw+\.? +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
	      (message "Matched %s" (match-string 1))
	      (setq ts (concat "[" (match-string 1) "]"))
	      (goto-char (match-end 1))
	      (setq org-clock-start-time
		    (apply 'encode-time
			   (org-parse-time-string (match-string 1))))
	      (setq org-clock-effort (org-get-effort))
	      (setq org-clock-total-time (org-clock-sum-current-item
					  (org-clock-get-sum-start))))
	     ((eq org-clock-in-resume 'auto-restart)
	      ;; called from org-clock-load during startup,
	      ;; do not interrupt, but warn!
	      (message "Cannot restart clock because task does not contain unfinished clock")
	      (ding)
	      (sit-for 2)
	      (throw 'abort nil))
	     (t
	      (insert-before-markers "\n")
	      (backward-char 1)
	      (org-indent-line-function)
	      (when (and (save-excursion
			   (end-of-line 0)
			   (org-in-item-p)))
		(beginning-of-line 1)
		(org-indent-line-to (- (org-get-indentation) 2)))
	      (insert org-clock-string " ")
	      (setq org-clock-effort (org-get-effort))
	      (setq org-clock-total-time (org-clock-sum-current-item
					  (org-clock-get-sum-start)))
	      (setq org-clock-start-time
		    (or (and leftover
			     (y-or-n-p
			      (format
			       "You stopped another clock %d mins ago; start this one from then? "
			       (/ (- (org-float-time (current-time))
				     (org-float-time leftover)) 60)))
			     leftover)
			start-time
			(current-time)))
	      (setq ts (org-insert-time-stamp org-clock-start-time
					      'with-hm 'inactive))))
	    (move-marker org-clock-marker (point) (buffer-base-buffer))
	    (move-marker org-clock-hd-marker
			 (save-excursion (org-back-to-heading t) (point))
			 (buffer-base-buffer))
	    (setq org-clock-has-been-used t)
	    (or global-mode-string (setq global-mode-string '("")))
	    (or (memq 'org-mode-line-string global-mode-string)
		(setq global-mode-string
		      (append global-mode-string '(org-mode-line-string))))
	    (org-clock-update-mode-line)
	    (when org-clock-mode-line-timer
	      (cancel-timer org-clock-mode-line-timer)
	      (setq org-clock-mode-line-timer nil))
	    (setq org-clock-mode-line-timer
		  (run-with-timer org-clock-update-period
				  org-clock-update-period
				  'org-clock-update-mode-line))
	    (when org-clock-idle-timer
	      (cancel-timer org-clock-idle-timer)
	      (setq org-clock-idle-timer nil))
	    (setq org-clock-idle-timer
		  (run-with-timer 60 60 'org-resolve-clocks-if-idle))
	    (message "Clock starts at %s - %s" ts msg-extra)
	    (run-hooks 'org-clock-in-hook)))))))

(defun org-clock-mark-default-task ()
  "Mark current task as default task."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (move-marker org-clock-default-task (point))))

(defvar msg-extra)
(defun org-clock-get-sum-start ()
  "Return the time from which clock times should be counted.
This is for the currently running clock as it is displayed
in the mode line.  This function looks at the properties
LAST_REPEAT and in particular CLOCK_MODELINE_TOTAL and the
corresponding variable `org-clock-modeline-total' and then
decides which time to use."
  (let ((cmt (or (org-entry-get nil "CLOCK_MODELINE_TOTAL")
		 (symbol-name org-clock-modeline-total)))
	(lr (org-entry-get nil "LAST_REPEAT")))
    (cond
     ((equal cmt "current")
      (setq msg-extra "showing time in current clock instance")
      (current-time))
     ((equal cmt "today")
      (setq msg-extra "showing today's task time.")
      (let* ((dt (decode-time (current-time))))
	(setq dt (append (list 0 0 0) (nthcdr 3 dt)))
	(if org-extend-today-until
	    (setf (nth 2 dt) org-extend-today-until))
	(apply 'encode-time dt)))
     ((or (equal cmt "all")
	  (and (or (not cmt) (equal cmt "auto"))
	       (not lr)))
      (setq msg-extra "showing entire task time.")
      nil)
     ((or (equal cmt "repeat")
	  (and (or (not cmt) (equal cmt "auto"))
	       lr))
      (setq msg-extra "showing task time since last repeat.")
      (if (not lr)
	  nil
	(org-time-string-to-time lr)))
     (t nil))))

(defun org-clock-find-position (find-unclosed)
  "Find the location where the next clock line should be inserted.
When FIND-UNCLOSED is non-nil, first check if there is an unclosed clock
line and position cursor in that line."
  (org-back-to-heading t)
  (catch 'exit
    (let* ((org-clock-into-drawer (org-clock-into-drawer))
	   (beg (save-excursion
		  (beginning-of-line 2)
		  (or (bolp) (newline))
		  (point)))
	   (end (progn (outline-next-heading) (point)))
	   (re (concat "^[ \t]*" org-clock-string))
	   (cnt 0)
	   (drawer (if (stringp org-clock-into-drawer)
		       org-clock-into-drawer "LOGBOOK"))
	   first last ind-last)
      (goto-char beg)
      (when (and find-unclosed
		 (re-search-forward
		  (concat "^[ \t]*" org-clock-string
			  " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
			  " *\\sw+ +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")
		  end t))
	(beginning-of-line 1)
	(throw 'exit t))
      (when (eobp) (newline) (setq end (max (point) end)))
      (when (re-search-forward (concat "^[ \t]*:" drawer ":") end t)
	;; we seem to have a CLOCK drawer, so go there.
	(beginning-of-line 2)
	(or org-log-states-order-reversed
	    (and (re-search-forward org-property-end-re nil t)
		 (goto-char (match-beginning 0))))
	(throw 'exit t))
      ;; Lets count the CLOCK lines
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq first (or first (match-beginning 0))
	      last (match-beginning 0)
	      cnt (1+ cnt)))
      (when (and (integerp org-clock-into-drawer)
		 last
		 (>= (1+ cnt) org-clock-into-drawer))
	;; Wrap current entries into a new drawer
	(goto-char last)
	(setq ind-last (org-get-indentation))
	(beginning-of-line 2)
	(if (and (>= (org-get-indentation) ind-last)
		 (org-at-item-p))
	    (when (and (>= (org-get-indentation) ind-last)
		   (org-at-item-p))
	      (let ((struct (org-list-struct)))
		(goto-char (org-list-get-bottom-point struct)))))
	(insert ":END:\n")
	(beginning-of-line 0)
	(org-indent-line-to ind-last)
	(goto-char first)
	(insert ":" drawer ":\n")
	(beginning-of-line 0)
	(org-indent-line-function)
	(org-flag-drawer t)
	(beginning-of-line 2)
	(or org-log-states-order-reversed
	    (and (re-search-forward org-property-end-re nil t)
		 (goto-char (match-beginning 0))))
	(throw 'exit nil))

      (goto-char beg)
      (while (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		  (not (equal (match-string 1) org-clock-string)))
	;; Planning info, skip to after it
	(beginning-of-line 2)
	(or (bolp) (newline)))
      (when (or (eq org-clock-into-drawer t)
		(stringp org-clock-into-drawer)
		(and (integerp org-clock-into-drawer)
		     (< org-clock-into-drawer 2)))
	(insert ":" drawer ":\n:END:\n")
	(beginning-of-line -1)
	(org-indent-line-function)
	(org-flag-drawer t)
	(beginning-of-line 2)
	(org-indent-line-function)
	(beginning-of-line)
	(or org-log-states-order-reversed
	    (and (re-search-forward org-property-end-re nil t)
		 (goto-char (match-beginning 0))))))))

(defun org-clock-out (&optional fail-quietly at-time)
  "Stop the currently running clock.
If there is no running clock, throw an error, unless FAIL-QUIETLY is set."
  (interactive)
  (catch 'exit
    (when (not (org-clocking-p))
      (setq global-mode-string
	    (delq 'org-mode-line-string global-mode-string))
      (force-mode-line-update)
      (if fail-quietly (throw 'exit t) (error "No active clock")))
    (let (ts te s h m remove)
      (save-excursion ; Do not replace this with `with-current-buffer'.
	(with-no-warnings (set-buffer (org-clocking-buffer)))
	(save-restriction
	  (widen)
	  (goto-char org-clock-marker)
	  (beginning-of-line 1)
	  (if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		   (equal (match-string 1) org-clock-string))
	      (setq ts (match-string 2))
	    (if fail-quietly (throw 'exit nil) (error "Clock start time is gone")))
	  (goto-char (match-end 0))
	  (delete-region (point) (point-at-eol))
	  (insert "--")
	  (setq te (org-insert-time-stamp (or at-time (current-time))
					  'with-hm 'inactive))
	  (setq s (- (org-float-time (apply 'encode-time (org-parse-time-string te)))
		     (org-float-time (apply 'encode-time (org-parse-time-string ts))))
		h (floor (/ s 3600))
		s (- s (* 3600 h))
		m (floor (/ s 60))
		s (- s (* 60 s)))
	  (insert " => " (format "%2d:%02d" h m))
	  (when (setq remove (and org-clock-out-remove-zero-time-clocks
				  (= (+ h m) 0)))
	    (beginning-of-line 1)
	    (delete-region (point) (point-at-eol))
	    (and (looking-at "\n") (> (point-max) (1+ (point)))
		 (delete-char 1)))
	  (move-marker org-clock-marker nil)
	  (move-marker org-clock-hd-marker nil)
	  (when org-log-note-clock-out
	    (org-add-log-setup 'clock-out nil nil nil nil
			       (concat "# Task: " (org-get-heading t) "\n\n")))
	  (when org-clock-mode-line-timer
	    (cancel-timer org-clock-mode-line-timer)
	    (setq org-clock-mode-line-timer nil))
	  (when org-clock-idle-timer
	    (cancel-timer org-clock-idle-timer)
	    (setq org-clock-idle-timer nil))
	  (setq global-mode-string
		(delq 'org-mode-line-string global-mode-string))
	  (when org-clock-out-switch-to-state
	    (save-excursion
	      (org-back-to-heading t)
	      (let ((org-inhibit-logging t)
		    (org-clock-out-when-done nil))
		(cond
		 ((functionp org-clock-out-switch-to-state)
		  (looking-at org-complex-heading-regexp)
		  (let ((newstate (funcall org-clock-out-switch-to-state
					   (match-string 2))))
		    (if newstate (org-todo newstate))))
		 ((and org-clock-out-switch-to-state
		       (not (looking-at (concat org-outline-regexp "[ \t]*"
						org-clock-out-switch-to-state
						"\\>"))))
		  (org-todo org-clock-out-switch-to-state))))))
	  (force-mode-line-update)
	  (message (concat "Clock stopped at %s after HH:MM = " org-time-clocksum-format "%s") te h m
		   (if remove " => LINE REMOVED" ""))
          (run-hooks 'org-clock-out-hook)
	  (org-clock-delete-current))))))

(add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer)

(defun org-clock-remove-empty-clock-drawer nil
  "Remove empty clock drawer in the current subtree."
  (let* ((olid (or (org-entry-get (point) "LOG_INTO_DRAWER")
		   org-log-into-drawer))
	 (clock-drawer (if (eq t olid) "LOGBOOK" olid))
	 (end (save-excursion (org-end-of-subtree t t))))
    (when clock-drawer
      (save-excursion
	(org-back-to-heading t)
	(while (search-forward clock-drawer end t)
	  (goto-char (match-beginning 0))
	  (org-remove-empty-drawer-at clock-drawer (point))
	  (forward-line 1))))))

(defun org-at-clock-log-p nil
  "Is the cursor on the clock log line?"
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at "^[ \t]*CLOCK:")))

(defun org-clock-timestamps-up nil
  "Increase CLOCK timestamps at cursor."
  (interactive)
  (org-clock-timestamps-change 'up))

(defun org-clock-timestamps-down nil
  "Increase CLOCK timestamps at cursor."
  (interactive)
  (org-clock-timestamps-change 'down))

(defun org-clock-timestamps-change (updown)
  "Change CLOCK timestamps synchronously at cursor.
UPDOWN tells whether to change 'up or 'down."
  (setq org-ts-what nil)
  (when (org-at-timestamp-p t)
    (let ((tschange (if (eq updown 'up) 'org-timestamp-up
		      'org-timestamp-down))
	  ts1 begts1 ts2 begts2 updatets1 tdiff)
      (save-excursion
	(move-beginning-of-line 1)
	(re-search-forward org-ts-regexp3 nil t)
	(setq ts1 (match-string 0) begts1 (match-beginning 0))
	(when (re-search-forward org-ts-regexp3 nil t)
	  (setq ts2 (match-string 0) begts2 (match-beginning 0))))
      ;; Are we on the second timestamp?
      (if (<= begts2 (point)) (setq updatets1 t))
      (if (not ts2)
	  ;; fall back on org-timestamp-up if there is only one
	  (funcall tschange)
	;; setq this so that (boundp 'org-ts-what is non-nil)
	(funcall tschange)
	(let ((ts (if updatets1 ts2 ts1))
	      (begts (if updatets1 begts1 begts2)))
	  (setq tdiff
		(subtract-time
		 (org-time-string-to-time org-last-changed-timestamp)
		 (org-time-string-to-time ts)))
	  (save-excursion
	    (goto-char begts)
	    (org-timestamp-change
	     (round (/ (org-float-time tdiff)
		       (cond ((eq org-ts-what 'minute) 60)
			     ((eq org-ts-what 'hour) 3600)
			     ((eq org-ts-what 'day) (* 24 3600))
			     ((eq org-ts-what 'month) (* 24 3600 31))
			     ((eq org-ts-what 'year) (* 24 3600 365.2)))))
	     org-ts-what 'updown)))))))

(defun org-clock-cancel ()
  "Cancel the running clock by removing the start timestamp."
  (interactive)
  (when (not (org-clocking-p))
    (setq global-mode-string
         (delq 'org-mode-line-string global-mode-string))
    (force-mode-line-update)
    (error "No active clock"))
  (save-excursion ; Do not replace this with `with-current-buffer'.
    (with-no-warnings (set-buffer (org-clocking-buffer)))
    (goto-char org-clock-marker)
    (delete-region (1- (point-at-bol)) (point-at-eol))
    ;; Just in case, remove any empty LOGBOOK left over
    (org-remove-empty-drawer-at "LOGBOOK" (point)))
  (move-marker org-clock-marker nil)
  (move-marker org-clock-hd-marker nil)
  (setq global-mode-string
	(delq 'org-mode-line-string global-mode-string))
  (force-mode-line-update)
  (message "Clock canceled")
  (run-hooks 'org-clock-cancel-hook))

(defun org-clock-goto (&optional select)
  "Go to the currently clocked-in entry, or to the most recently clocked one.
With prefix arg SELECT, offer recently clocked tasks for selection."
  (interactive "@P")
  (let* ((recent nil)
	 (m (cond
	     (select
	      (or (org-clock-select-task "Select task to go to: ")
		  (error "No task selected")))
	     ((org-clocking-p) org-clock-marker)
	     ((and org-clock-goto-may-find-recent-task
		   (car org-clock-history)
		   (marker-buffer (car org-clock-history)))
	      (setq recent t)
	      (car org-clock-history))
	     (t (error "No active or recent clock task")))))
    (org-pop-to-buffer-same-window (marker-buffer m))
    (if (or (< m (point-min)) (> m (point-max))) (widen))
    (goto-char m)
    (org-show-entry)
    (org-back-to-heading t)
    (org-cycle-hide-drawers 'children)
    (recenter)
    (org-reveal)
    (if recent
	(message "No running clock, this is the most recently clocked task"))
    (run-hooks 'org-clock-goto-hook)))

(defvar org-clock-file-total-minutes nil
  "Holds the file total time in minutes, after a call to `org-clock-sum'.")
(make-variable-buffer-local 'org-clock-file-total-minutes)

(defun org-clock-sum (&optional tstart tend headline-filter)
  "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.  HEADLINE-FILTER is a
zero-arg function that, if specified, is called for each headline in the time
range with point at the headline.  Headlines for which HEADLINE-FILTER returns
nil are excluded from the clock summation."
  (interactive)
  (let* ((bmp (buffer-modified-p))
	 (re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		     org-clock-string
		     "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	 (lmax 30)
	 (ltimes (make-vector lmax 0))
	 (t1 0)
	 (level 0)
	 ts te dt
	 time)
    (if (stringp tstart) (setq tstart (org-time-string-to-seconds tstart)))
    (if (stringp tend) (setq tend (org-time-string-to-seconds tend)))
    (if (consp tstart) (setq tstart (org-float-time tstart)))
    (if (consp tend) (setq tend (org-float-time tend)))
    (remove-text-properties (point-min) (point-max)
                            '(:org-clock-minutes t
                              :org-clock-force-headline-inclusion t))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward re nil t)
	(cond
	 ((match-end 2)
	  ;; Two time stamps
	  (setq ts (match-string 2)
		te (match-string 3)
		ts (org-float-time
		    (apply 'encode-time (org-parse-time-string ts)))
		te (org-float-time
		    (apply 'encode-time (org-parse-time-string te)))
		ts (if tstart (max ts tstart) ts)
		te (if tend (min te tend) te)
		dt (- te ts)
		t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1)))
	 ((match-end 4)
	  ;; A naked time
	  (setq t1 (+ t1 (string-to-number (match-string 5))
		      (* 60 (string-to-number (match-string 4))))))
	 (t ;; A headline
	  ;; Add the currently clocking item time to the total
	  (when (and org-clock-report-include-clocking-task
		     (equal (org-clocking-buffer) (current-buffer))
		     (equal (marker-position org-clock-hd-marker) (point))
		     tstart
		     tend
		     (>= (org-float-time org-clock-start-time) tstart)
		     (<= (org-float-time org-clock-start-time) tend))
	    (let ((time (floor (- (org-float-time)
				  (org-float-time org-clock-start-time)) 60)))
	      (setq t1 (+ t1 time))))
	  (let* ((headline-forced
		  (get-text-property (point)
                                     :org-clock-force-headline-inclusion))
                 (headline-included
                  (or (null headline-filter)
                      (save-excursion
                        (save-match-data (funcall headline-filter))))))
	    (setq level (- (match-end 1) (match-beginning 1)))
	    (when (or (> t1 0) (> (aref ltimes level) 0))
	      (when (or headline-included headline-forced)
                (if headline-included
                    (loop for l from 0 to level do
                          (aset ltimes l (+ (aref ltimes l) t1))))
		(setq time (aref ltimes level))
		(goto-char (match-beginning 0))
		(put-text-property (point) (point-at-eol) :org-clock-minutes time)
                (if headline-filter
                    (save-excursion
                      (save-match-data
                        (while
                            (> (funcall outline-level) 1)
                          (outline-up-heading 1 t)
                          (put-text-property
                           (point) (point-at-eol)
                           :org-clock-force-headline-inclusion t))))))
	      (setq t1 0)
	      (loop for l from level to (1- lmax) do
		    (aset ltimes l 0)))))))
      (setq org-clock-file-total-minutes (aref ltimes 0)))
    (set-buffer-modified-p bmp)))

(defun org-clock-sum-current-item (&optional tstart)
  "Return time, clocked on current item in total."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum tstart)
      org-clock-file-total-minutes)))

(defun org-clock-display (&optional total-only)
  "Show subtree times in the entire buffer.
If TOTAL-ONLY is non-nil, only show the total time for the entire file
in the echo area.

Use \\[org-clock-remove-overlays] to remove the subtree times."
  (interactive)
  (org-clock-remove-overlays)
  (let (time h m p)
    (org-clock-sum)
    (unless total-only
      (save-excursion
	(goto-char (point-min))
	(while (or (and (equal (setq p (point)) (point-min))
			(get-text-property p :org-clock-minutes))
		   (setq p (next-single-property-change
			    (point) :org-clock-minutes)))
	  (goto-char p)
	  (when (setq time (get-text-property p :org-clock-minutes))
	    (org-clock-put-overlay time (funcall outline-level))))
	(setq h (/ org-clock-file-total-minutes 60)
	      m (- org-clock-file-total-minutes (* 60 h)))
	;; Arrange to remove the overlays upon next change.
	(when org-remove-highlights-with-change
	  (org-add-hook 'before-change-functions 'org-clock-remove-overlays
			nil 'local))))
    (if org-time-clocksum-use-fractional
	(message (concat "Total file time: " org-time-clocksum-fractional-format
			 " (%d hours and %d minutes)")
		 (/ (+ (* h 60.0) m) 60.0) h m)
      (message (concat "Total file time: " org-time-clocksum-format
		       " (%d hours and %d minutes)") h m h m))))

(defvar org-clock-overlays nil)
(make-variable-buffer-local 'org-clock-overlays)

(defun org-clock-put-overlay (time &optional level)
  "Put an overlays on the current line, displaying TIME.
If LEVEL is given, prefix time with a corresponding number of stars.
This creates a new overlay and stores it in `org-clock-overlays', so that it
will be easy to remove."
  (let* ((c 60) (h (floor (/ time 60))) (m (- time (* 60 h)))
	 (l (if level (org-get-valid-level level 0) 0))
	 (fmt (concat "%s " (if org-time-clocksum-use-fractional
				org-time-clocksum-fractional-format
			      org-time-clocksum-format) "%s"))
	 (off 0)
	 ov tx)
    (org-move-to-column c)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (setq ov (make-overlay (1- (point)) (point-at-eol))
	  tx (concat (buffer-substring (1- (point)) (point))
		     (make-string (+ off (max 0 (- c (current-column)))) ?.)
		     (org-add-props (if org-time-clocksum-use-fractional
					(format fmt
						(make-string l ?*)
						(/ (+ (* h 60.0) m) 60.0)
						(make-string (- 16 l) ?\ ))
				      (format fmt
					      (make-string l ?*) h m
					      (make-string (- 16 l) ?\ )))
			 (list 'face 'org-clock-overlay))
		     ""))
    (if (not (featurep 'xemacs))
	(overlay-put ov 'display tx)
      (overlay-put ov 'invisible t)
      (overlay-put ov 'end-glyph (make-glyph tx)))
    (push ov org-clock-overlays)))

(defun org-clock-remove-overlays (&optional beg end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc 'delete-overlay org-clock-overlays)
    (setq org-clock-overlays nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-clock-remove-overlays 'local))))

(defvar org-state) ;; dynamically scoped into this function
(defun org-clock-out-if-current ()
  "Clock out if the current entry contains the running clock.
This is used to stop the clock after a TODO entry is marked DONE,
and is only done if the variable `org-clock-out-when-done' is not nil."
  (when (and (org-clocking-p)
	     org-clock-out-when-done
	     (marker-buffer org-clock-marker)
	     (or (and (eq t org-clock-out-when-done)
		      (member org-state org-done-keywords))
		 (and (listp org-clock-out-when-done)
		      (member org-state org-clock-out-when-done)))
	     (equal (or (buffer-base-buffer (org-clocking-buffer))
			(org-clocking-buffer))
		    (or (buffer-base-buffer (current-buffer))
			(current-buffer)))
	     (< (point) org-clock-marker)
	     (> (save-excursion (outline-next-heading) (point))
		org-clock-marker))
    ;; Clock out, but don't accept a logging message for this.
    (let ((org-log-note-clock-out nil)
	  (org-clock-out-switch-to-state nil))
      (org-clock-out))))

(add-hook 'org-after-todo-state-change-hook
	  'org-clock-out-if-current)

;;;###autoload
(defun org-get-clocktable (&rest props)
  "Get a formatted clocktable with parameters according to PROPS.
The table is created in a temporary buffer, fully formatted and
fontified, and then returned."
  ;; Set the defaults
  (setq props (plist-put props :name "clocktable"))
  (unless (plist-member props :maxlevel)
    (setq props (plist-put props :maxlevel 2)))
  (unless (plist-member props :scope)
    (setq props (plist-put props :scope 'agenda)))
  (with-temp-buffer
    (org-mode)
    (org-create-dblock props)
    (org-update-dblock)
    (font-lock-fontify-buffer)
    (forward-line 2)
    (buffer-substring (point) (progn
				(re-search-forward "^[ \t]*#\\+END" nil t)
				(point-at-bol)))))

(defun org-clock-report (&optional arg)
  "Create a table containing a report about clocked time.
If the cursor is inside an existing clocktable block, then the table
will be updated.  If not, a new clocktable will be inserted.  The scope
of the new clock will be subtree when called from within a subtree, and
file elsewhere.

When called with a prefix argument, move to the first clock table in the
buffer and update it."
  (interactive "P")
  (org-clock-remove-overlays)
  (when arg
    (org-find-dblock "clocktable")
    (org-show-entry))
  (if (org-in-clocktable-p)
      (goto-char (org-in-clocktable-p))
    (let ((props (if (ignore-errors
		       (save-excursion (org-back-to-heading)))
		     (list :name "clocktable" :scope 'subtree)
		   (list :name "clocktable"))))
      (org-create-dblock
       (org-combine-plists org-clock-clocktable-default-properties props))))
  (org-update-dblock))

(defun org-day-of-week (day month year)
  "Returns the day of the week as an integer."
  (nth 6
       (decode-time
	(date-to-time
	 (format "%d-%02d-%02dT00:00:00" year month day)))))

(defun org-quarter-to-date (quarter year)
  "Get the date (week day year) of the first day of a given quarter."
  (let (startday)
    (cond
     ((= quarter 1)
      (setq startday (org-day-of-week 1 1 year))
      (cond
       ((= startday 0)
	(list 52 7 (- year 1)))
       ((= startday 6)
	(list 52 6 (- year 1)))
       ((<= startday 4)
	(list 1 startday year))
       ((> startday 4)
	(list 53 startday (- year 1)))
       )
      )
     ((= quarter 2)
      (setq startday (org-day-of-week 1 4 year))
      (cond
       ((= startday 0)
	(list 13 startday year))
       ((< startday 4)
	(list 14 startday year))
       ((>= startday 4)
	(list 13 startday year))
       )
      )
     ((= quarter 3)
      (setq startday (org-day-of-week 1 7 year))
      (cond
       ((= startday 0)
	(list 26 startday year))
       ((< startday 4)
	(list 27 startday year))
       ((>= startday 4)
	(list 26 startday year))
       )
      )
     ((= quarter 4)
      (setq startday (org-day-of-week 1 10 year))
      (cond
       ((= startday 0)
	(list 39 startday year))
       ((<= startday 4)
	(list 40 startday year))
       ((> startday 4)
	(list 39 startday year)))))))

(defun org-clock-special-range (key &optional time as-strings)
  "Return two times bordering a special time range.
Key is a symbol specifying the range and can be one of `today', `yesterday',
`thisweek', `lastweek', `thismonth', `lastmonth', `thisyear', `lastyear'.
A week starts Monday 0:00 and ends Sunday 24:00.
The range is determined relative to TIME.  TIME defaults to the current time.
The return value is a cons cell with two internal times like the ones
returned by `current time' or `encode-time'. if AS-STRINGS is non-nil,
the returned times will be formatted strings."
  (if (integerp key) (setq key (intern (number-to-string key))))
  (let* ((tm (decode-time (or time (current-time))))
	 (s 0) (m (nth 1 tm)) (h (nth 2 tm))
	 (d (nth 3 tm)) (month (nth 4 tm)) (y (nth 5 tm))
	 (dow (nth 6 tm))
	 (skey (symbol-name key))
	 (shift 0)
         (q (cond ((>= (nth 4 tm) 10) 4)
                  ((>= (nth 4 tm) 7) 3)
                  ((>= (nth 4 tm) 4) 2)
                  ((>= (nth 4 tm) 1) 1)))
	 s1 m1 h1 d1 month1 y1 diff ts te fm txt w date
	 interval tmp shiftedy shiftedm shiftedq)
    (cond
     ((string-match "^[0-9]+$" skey)
      (setq y (string-to-number skey) m 1 d 1 key 'year))
     ((string-match "^\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)$" skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d 1 key 'month))
     ((string-match "^\\([0-9]+\\)-[wW]\\([0-9]\\{1,2\\}\\)$" skey)
      (require 'cal-iso)
      (setq y (string-to-number (match-string 1 skey))
	    w (string-to-number (match-string 2 skey)))
      (setq date (calendar-gregorian-from-absolute
		  (calendar-absolute-from-iso (list w 1 y))))
      (setq d (nth 1 date) month (car date) y (nth 2 date)
	    dow 1
	    key 'week))
      ((string-match "^\\([0-9]+\\)-[qQ]\\([1-4]\\)$" skey)
       (require 'cal-iso)
       (setq y (string-to-number (match-string 1 skey)))
       (setq q (string-to-number (match-string 2 skey)))
       (setq date (calendar-gregorian-from-absolute
                   (calendar-absolute-from-iso (org-quarter-to-date q y))))
       (setq d (nth 1 date) month (car date) y (nth 2 date)
            dow 1
            key 'quarter))
     ((string-match "^\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)$" skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d (string-to-number (match-string 3 skey))
	    key 'day))
     ((string-match "\\([-+][0-9]+\\)$" skey)
      (setq shift (string-to-number (match-string 1 skey))
            key (intern (substring skey 0 (match-beginning 1))))
       (if(and (memq key '(quarter thisq)) (> shift 0))
         (error "Looking forward with quarters isn't implemented.")
        ())))

    (when (= shift 0)
       (cond ((eq key 'yesterday) (setq key 'today   shift -1))
            ((eq key 'lastweek)  (setq key 'week    shift -1))
            ((eq key 'lastmonth) (setq key 'month   shift -1))
            ((eq key 'lastyear)  (setq key 'year    shift -1))
            ((eq key 'lastq)     (setq key 'quarter shift -1))))
    (cond
     ((memq key '(day today))
      (setq d (+ d shift) h 0 m 0 h1 24 m1 0))
     ((memq key '(week thisweek))
      (setq diff (+ (* -7 shift) (if (= dow 0) 6 (1- dow)))
	    m 0 h 0 d (- d diff) d1 (+ 7 d)))
     ((memq key '(month thismonth))
      (setq d 1 h 0 m 0 d1 1 month (+ month shift) month1 (1+ month) h1 0 m1 0))
     ((memq key '(quarter thisq))
      ; compute if this shift remains in this year
      ; if not, compute how many years and quarters we have to shift (via floor*)
      ; and compute the shifted years, months and quarters
      (cond
       ((< (+ (- q 1) shift) 0) ; shift not in this year
       (setq interval (* -1 (+ (- q 1) shift)))
       ; set tmp to ((years to shift) (quarters to shift))
       (setq tmp (org-floor* interval 4))
       ; due to the use of floor, 0 quarters actually means 4
       (if (= 0 (nth 1 tmp))
           (setq shiftedy (- y (nth 0 tmp))
                 shiftedm 1
                 shiftedq 1)
         (setq shiftedy (- y (+ 1 (nth 0 tmp)))
               shiftedm (- 13 (* 3 (nth 1 tmp)))
               shiftedq (- 5 (nth 1 tmp))))
       (setq d 1 h 0 m 0 d1 1 month shiftedm month1 (+ 3 shiftedm) h1 0 m1 0 y shiftedy))
       ((> (+ q shift) 0) ; shift is within this year
       (setq shiftedq (+ q shift))
       (setq shiftedy y)
       (setq d 1 h 0 m 0 d1 1 month (+ 1 (* 3 (- (+ q shift) 1))) month1 (+ 4 (* 3 (- (+ q shift) 1))) h1 0 m1 0))))
     ((memq key '(year thisyear))
      (setq m 0 h 0 d 1 month 1 y (+ y shift) y1 (1+ y)))
     (t (error "No such time block %s" key)))
    (setq ts (encode-time s m h d month y)
	  te (encode-time (or s1 s) (or m1 m) (or h1 h)
			  (or d1 d) (or month1 month) (or y1 y)))
    (setq fm (cdr org-time-stamp-formats))
    (cond
     ((memq key '(day today))
      (setq txt (format-time-string "%A, %B %d, %Y" ts)))
     ((memq key '(week thisweek))
      (setq txt (format-time-string "week %G-W%V" ts)))
     ((memq key '(month thismonth))
      (setq txt (format-time-string "%B %Y" ts)))
     ((memq key '(year thisyear))
      (setq txt (format-time-string "the year %Y" ts)))
     ((memq key '(quarter thisq))
      (setq txt (concatenate 'string (org-count-quarter shiftedq) " quarter of " (number-to-string shiftedy))))
     )
    (if as-strings
	(list (format-time-string fm ts) (format-time-string fm te) txt)
      (list ts te txt))))

(defun org-count-quarter (n)
  (cond
   ((= n 1) "1st")
   ((= n 2) "2nd")
   ((= n 3) "3rd")
   ((= n 4) "4th")))

(defun org-clocktable-shift (dir n)
  "Try to shift the :block date of the clocktable at point.
Point must be in the #+BEGIN: line of a clocktable, or this function
will throw an error.
DIR is a direction, a symbol `left', `right', `up', or `down'.
Both `left' and `down' shift the block toward the past, `up' and `right'
push it toward the future.
N is the number of shift steps to take.  The size of the step depends on
the currently selected interval size."
  (setq n (prefix-numeric-value n))
  (and (memq dir '(left down)) (setq n (- n)))
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (looking-at "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>.*?:block[ \t]+\\(\\S-+\\)"))
	(error "Line needs a :block definition before this command works")
      (let* ((b (match-beginning 1)) (e (match-end 1))
	     (s (match-string 1))
	     block shift ins y mw d date wp m)
	(cond
	 ((equal s "yesterday") (setq s "today-1"))
	 ((equal s "lastweek") (setq s "thisweek-1"))
	 ((equal s "lastmonth") (setq s "thismonth-1"))
	 ((equal s "lastyear") (setq s "thisyear-1"))
	 ((equal s "lastq") (setq s "thisq-1")))

       (cond
        ((string-match "^\\(today\\|thisweek\\|thismonth\\|thisyear\\|thisq\\)\\([-+][0-9]+\\)?$" s)
         (setq block (match-string 1 s)
               shift (if (match-end 2)
                         (string-to-number (match-string 2 s))
                       0))
         (setq shift (+ shift n))
         (setq ins (if (= shift 0) block (format "%s%+d" block shift))))
	((string-match "\\([0-9]+\\)\\(-\\([wWqQ]?\\)\\([0-9]\\{1,2\\}\\)\\(-\\([0-9]\\{1,2\\}\\)\\)?\\)?" s)
        ;;               1        1  2   3       3  4                  4  5   6                6  5   2
         (setq y (string-to-number (match-string 1 s))
               wp (and (match-end 3) (match-string 3 s))
               mw (and (match-end 4) (string-to-number (match-string 4 s)))
	       d (and (match-end 6) (string-to-number (match-string 6 s))))
	 (cond
	  (d (setq ins (format-time-string
                        "%Y-%m-%d"
                        (encode-time 0 0 0 (+ d n) m y))))
          ((and wp (string-match "w\\|W" wp) mw (> (length wp) 0))
           (require 'cal-iso)
           (setq date (calendar-gregorian-from-absolute
		       (calendar-absolute-from-iso (list (+ mw n) 1 y))))
           (setq ins (format-time-string
                      "%G-W%V"
                      (encode-time 0 0 0 (nth 1 date) (car date) (nth 2 date)))))
	  ((and wp (string-match "q\\|Q" wp) mw (> (length wp) 0))
           (require 'cal-iso)
	   ; if the 4th + 1 quarter is requested we flip to the 1st quarter of the next year
           (if (> (+ mw n) 4)
               (setq mw 0
                     y (+ 1 y))
	     ())
	   ; if the 1st - 1 quarter is requested we flip to the 4th quarter of the previous year
           (if (= (+ mw n) 0)
               (setq mw 5
                     y (- y 1))
             ())
           (setq date (calendar-gregorian-from-absolute
		       (calendar-absolute-from-iso (org-quarter-to-date (+ mw n) y))))
           (setq ins (format-time-string
                      (concatenate 'string (number-to-string y) "-Q" (number-to-string (+ mw n)))
                      (encode-time 0 0 0 (nth 1 date) (car date) (nth 2 date)))))
          (mw
           (setq ins (format-time-string
                      "%Y-%m"
		      (encode-time 0 0 0 1 (+ mw n) y))))
	  (y
	   (setq ins (number-to-string (+ y n))))))
	(t (error "Cannot shift clocktable block")))
       (when ins
	 (goto-char b)
	 (insert ins)
	 (delete-region (point) (+ (point) (- e b)))
	 (beginning-of-line 1)
	 (org-update-dblock)
	 t)))))

(defun org-dblock-write:clocktable (params)
  "Write the standard clocktable."
  (setq params (org-combine-plists org-clocktable-defaults params))
  (catch 'exit
    (let* ((scope (plist-get params :scope))
	   (block (plist-get params :block))
	   (ts (plist-get params :tstart))
	   (te (plist-get params :tend))
	   (link (plist-get params :link))
	   (maxlevel (or (plist-get params :maxlevel) 3))
	   (step (plist-get params :step))
	   (timestamp (plist-get params :timestamp))
	   (formatter (or (plist-get params :formatter)
			  org-clock-clocktable-formatter
			  'org-clocktable-write-default))
	   cc range-text ipos pos one-file-with-archives
	   scope-is-list tbls level)
      ;; Check if we need to do steps
      (when block
	;; Get the range text for the header
	(setq cc (org-clock-special-range block nil t)
	      ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
      (when step
	;; Write many tables, in steps
	(unless (or block (and ts te))
	  (error "Clocktable `:step' can only be used with `:block' or `:tstart,:end'"))
	(org-clocktable-steps params)
	(throw 'exit nil))

      (setq ipos (point)) ; remember the insertion position

      ;; Get the right scope
      (setq pos (point))
      (cond
       ((and scope (listp scope) (symbolp (car scope)))
	(setq scope (eval scope)))
       ((eq scope 'agenda)
	(setq scope (org-agenda-files t)))
       ((eq scope 'agenda-with-archives)
	(setq scope (org-agenda-files t))
	(setq scope (org-add-archive-files scope)))
       ((eq scope 'file-with-archives)
	(setq scope (org-add-archive-files (list (buffer-file-name)))
	      one-file-with-archives t)))
      (setq scope-is-list (and scope (listp scope)))
      (if scope-is-list
	  ;; we collect from several files
	  (let* ((files scope)
		 file)
	    (org-prepare-agenda-buffers files)
	    (while (setq file (pop files))
	      (with-current-buffer (find-buffer-visiting file)
		(save-excursion
		  (save-restriction
		    (push (org-clock-get-table-data file params) tbls))))))
	;; Just from the current file
	(save-restriction
	  ;; get the right range into the restriction
	  (org-prepare-agenda-buffers (list (buffer-file-name)))
	  (cond
	   ((not scope))  ; use the restriction as it is now
	   ((eq scope 'file) (widen))
	   ((eq scope 'subtree) (org-narrow-to-subtree))
	   ((eq scope 'tree)
	    (while (org-up-heading-safe))
	    (org-narrow-to-subtree))
	   ((and (symbolp scope) (string-match "^tree\\([0-9]+\\)$"
					       (symbol-name scope)))
	    (setq level (string-to-number (match-string 1 (symbol-name scope))))
	    (catch 'exit
	      (while (org-up-heading-safe)
		(looking-at org-outline-regexp)
		(if (<= (org-reduced-level (funcall outline-level)) level)
		    (throw 'exit nil))))
	    (org-narrow-to-subtree)))
	  ;; do the table, with no file name.
	  (push (org-clock-get-table-data nil params) tbls)))

      ;; OK, at this point we tbls as a list of tables, one per file
      (setq tbls (nreverse tbls))

      (setq params (plist-put params :multifile scope-is-list))
      (setq params (plist-put params :one-file-with-archives
			      one-file-with-archives))

      (funcall formatter ipos tbls params))))

(defun org-clocktable-write-default (ipos tables params)
  "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
  ;; This function looks quite complicated, mainly because there are a
  ;; lot of options which can add or remove columns.  I have massively
  ;; commented this function, the I hope it is understandable.  If
  ;; someone wants to write their own special formatter, this maybe
  ;; much easier because there can be a fixed format with a
  ;; well-defined number of columns...
  (let* ((hlchars '((1 . "*") (2 . "/")))
	 (lwords (assoc (or (plist-get params :lang)
			    org-export-default-language)
			org-clock-clocktable-language-setup))
	 (multifile (plist-get params :multifile))
	 (block (plist-get params :block))
	 (ts (plist-get params :tstart))
	 (te (plist-get params :tend))
	 (header (plist-get  params :header))
	 (narrow (plist-get params :narrow))
	 (link (plist-get params :link))
	 (maxlevel (or (plist-get params :maxlevel) 3))
	 (emph (plist-get params :emphasize))
	 (level-p (plist-get params :level))
	 (timestamp (plist-get params :timestamp))
	 (properties (plist-get params :properties))
	 (ntcol (max 1 (or (plist-get params :tcolumns) 100)))
	 (rm-file-column (plist-get params :one-file-with-archives))
	 (indent (plist-get params :indent))
	 range-text total-time tbl level hlc formula pcol
	 file-time entries entry headline
	 recalc content narrow-cut-p tcol)

    ;; Implement abbreviations
    (when (plist-get params :compact)
      (setq level nil indent t narrow (or narrow '40!) ntcol 1))

    ;; Some consistency test for parameters
      (unless (integerp ntcol)
	(setq params (plist-put params :tcolumns (setq ntcol 100))))

      (when (and narrow (integerp narrow) link)
	;; We cannot have both integer narrow and link
	(message
	 "Using hard narrowing in clocktable to allow for links")
	(setq narrow (intern (format "%d!" narrow))))

      (when narrow
	(cond
	 ((integerp narrow))
	 ((and (symbolp narrow)
	       (string-match "\\`[0-9]+!\\'" (symbol-name narrow)))
	  (setq narrow-cut-p t
		narrow (string-to-number (substring (symbol-name narrow)
						    0 -1))))
	 (t
	  (error "Invalid value %s of :narrow property in clock table"
		 narrow))))

      (when block
	;; Get the range text for the header
	(setq range-text (nth 2 (org-clock-special-range block nil t))))

      ;; Compute the total time
      (setq total-time (apply '+ (mapcar 'cadr tables)))

      ;; Now we need to output this tsuff
      (goto-char ipos)

      ;; Insert the text *before* the actual table
      (insert-before-markers
       (or header
	   ;; Format the standard header
	   (concat
	    (nth 9 lwords) " ["
	    (substring
	     (format-time-string (cdr org-time-stamp-formats))
	     1 -1)
	    "]"
	    (if block (concat ", for " range-text ".") "")
	    "\n\n")))

      ;; Insert the narrowing line
      (when (and narrow (integerp narrow) (not narrow-cut-p))
	(insert-before-markers
	 "|"                            ; table line starter
	 (if multifile "|" "")          ; file column, maybe
	 (if level-p   "|" "")          ; level column, maybe
	 (if timestamp "|" "")          ; timestamp column, maybe
	 (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
	 (format "<%d>| |\n" narrow)))  ; headline and time columns

      ;; Insert the table header line
      (insert-before-markers
       "|"                              ; table line starter
       (if multifile (concat (nth 1 lwords) "|") "")  ; file column, maybe
       (if level-p   (concat (nth 2 lwords) "|") "")  ; level column, maybe
       (if timestamp (concat (nth 3 lwords) "|") "")  ; timestamp column, maybe
       (if properties (concat (mapconcat 'identity properties "|") "|") "") ;properties columns, maybe
       (concat (nth 4 lwords) "|"
	       (nth 5 lwords) "|\n"))                 ; headline and time columns

      ;; Insert the total time in the table
      (insert-before-markers
       "|-\n"                            ; a hline
       "|"                               ; table line starter
       (if multifile (concat "| " (nth 6 lwords) " ") "")
				         ; file column, maybe
       (if level-p   "|"      "")        ; level column, maybe
       (if timestamp "|"      "")        ; timestamp column, maybe
       (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
       (concat "*" (nth 7 lwords) "*| ") ; instead of a headline
       "*"
       (org-minutes-to-hh:mm-string (or total-time 0)) ; the time
       "*|\n")                          ; close line

      ;; Now iterate over the tables and insert the data
      ;; but only if any time has been collected
      (when (and total-time (> total-time 0))

	(while (setq tbl (pop tables))
	  ;; now tbl is the table resulting from one file.
	  (setq file-time (nth 1 tbl))
	  (when (or (and file-time (> file-time 0))
		    (not (plist-get params :fileskip0)))
	    (insert-before-markers "|-\n")  ; a hline because a new file starts
	    ;; First the file time, if we have multiple files
	    (when multifile
	      ;; Summarize the time collected from this file
	      (insert-before-markers
	       (format (concat "| %s %s | %s%s*" (nth 8 lwords) "* | *%s*|\n")
		       (file-name-nondirectory (car tbl))
		       (if level-p   "| " "") ; level column, maybe
		       (if timestamp "| " "") ; timestamp column, maybe
		       (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
		       (org-minutes-to-hh:mm-string (nth 1 tbl))))) ; the time

	    ;; Get the list of node entries and iterate over it
	    (setq entries (nth 2 tbl))
	    (while (setq entry (pop entries))
	      (setq level (car entry)
		    headline (nth 1 entry)
		    hlc (if emph (or (cdr (assoc level hlchars)) "") ""))
	      (when narrow-cut-p
		(if (and (string-match (concat "\\`" org-bracket-link-regexp
					       "\\'")
				       headline)
			 (match-end 3))
		    (setq headline
			  (format "[[%s][%s]]"
				  (match-string 1 headline)
				  (org-shorten-string (match-string 3 headline)
						      narrow)))
		  (setq headline (org-shorten-string headline narrow))))
	      (insert-before-markers
	       "|"                      ; start the table line
	       (if multifile "|" "")    ; free space for file name column?
	       (if level-p (format "%d|" (car entry)) "")   ; level, maybe
	       (if timestamp (concat (nth 2 entry) "|") "") ; timestamp, maybe
	       (if properties
		   (concat
		    (mapconcat
		     (lambda (p) (or (cdr (assoc p (nth 4 entry))) ""))
		     properties "|") "|") "")  ;properties columns, maybe
	       (if indent (org-clocktable-indent-string level) "") ; indentation
	       hlc headline hlc "|"                                ; headline
	       (make-string (min (1- ntcol) (or (- level 1))) ?|)
					; empty fields for higher levels
	       hlc (org-minutes-to-hh:mm-string (nth 3 entry)) hlc ; time
	       "|\n"                                               ; close line
	       )))))
      (backward-delete-char 1)
      (if (setq formula (plist-get params :formula))
	  (cond
	   ((eq formula '%)
	    ;; compute the column where the % numbers need to go
	    (setq pcol (+ 2
			  (if multifile 1 0)
			  (if level-p 1 0)
			  (if timestamp 1 0)
			  (min maxlevel (or ntcol 100))))
	    ;; compute the column where the total time is
	    (setq tcol (+ 2
			  (if multifile 1 0)
			  (if level-p 1 0)
			  (if timestamp 1 0)))
	    (insert
	     (format
	      "\n#+TBLFM: $%d='(org-clock-time%% @%d$%d $%d..$%d);%%.1f"
	      pcol            ; the column where the % numbers should go
	      (if (and narrow (not narrow-cut-p)) 3 2) ; row of the total time
	      tcol            ; column of the total time
	      tcol (1- pcol)  ; range of columns where times can be found
	      ))
	    (setq recalc t))
	   ((stringp formula)
	    (insert "\n#+TBLFM: " formula)
	    (setq recalc t))
	   (t (error "invalid formula in clocktable")))
	;; Should we rescue an old formula?
	(when (stringp (setq content (plist-get params :content)))
	  (when (string-match "^\\([ \t]*#\\+TBLFM:.*\\)" content)
	    (setq recalc t)
	    (insert "\n" (match-string 1 (plist-get params :content)))
	    (beginning-of-line 0))))
      ;; Back to beginning, align the table, recalculate if necessary
      (goto-char ipos)
      (skip-chars-forward "^|")
      (org-table-align)
      (when org-hide-emphasis-markers
	;; we need to align a second time
	(org-table-align))
      (when recalc
	(if (eq formula '%)
	    (save-excursion
	      (if (and narrow (not narrow-cut-p)) (beginning-of-line 2))
	      (org-table-goto-column pcol nil 'force)
	      (insert "%")))
	(org-table-recalculate 'all))
      (when rm-file-column
	;; The file column is actually not wanted
	(forward-char 1)
	(org-table-delete-column))
      total-time))

(defun org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "\\__"))
      (while (> level 2)
	(setq level (1- level)
	      str (concat str "___")))
      (concat str " "))))

(defun org-clocktable-steps (params)
  "Step through the range to make a number of clock tables."
  (let* ((p1 (copy-sequence params))
	 (ts (plist-get p1 :tstart))
	 (te (plist-get p1 :tend))
	 (step0 (plist-get p1 :step))
	 (step (cdr (assoc step0 '((day . 86400) (week . 604800)))))
	 (stepskip0 (plist-get p1 :stepskip0))
	 (block (plist-get p1 :block))
	 cc range-text step-time)
    (when block
      (setq cc (org-clock-special-range block nil t)
	    ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
    (cond
     ((numberp ts)
      ;; If ts is a number, it's an absolute day number from org-agenda.
      (destructuring-bind (month day year) (calendar-gregorian-from-absolute ts)
	(setq ts (org-float-time (encode-time 0 0 0 day month year)))))
     (ts
      (setq ts (org-float-time
		(apply 'encode-time (org-parse-time-string ts))))))
    (cond
     ((numberp te)
      ;; Likewise for te.
      (destructuring-bind (month day year) (calendar-gregorian-from-absolute te)
	(setq te (org-float-time (encode-time 0 0 0 day month year)))))
     (te
      (setq te (org-float-time
		(apply 'encode-time (org-parse-time-string te))))))
    (setq p1 (plist-put p1 :header ""))
    (setq p1 (plist-put p1 :step nil))
    (setq p1 (plist-put p1 :block nil))
    (while (< ts te)
      (or (bolp) (insert "\n"))
      (setq p1 (plist-put p1 :tstart (format-time-string
				      (org-time-stamp-format nil t)
				      (seconds-to-time ts))))
      (setq p1 (plist-put p1 :tend (format-time-string
				    (org-time-stamp-format nil t)
				    (seconds-to-time (setq ts (+ ts step))))))
      (insert "\n" (if (eq step0 'day) "Daily report: "
		     "Weekly report starting on: ")
	      (plist-get p1 :tstart) "\n")
      (setq step-time (org-dblock-write:clocktable p1))
      (re-search-forward "^[ \t]*#\\+END:")
      (when (and (equal step-time 0) stepskip0)
	;; Remove the empty table
	(delete-region (point-at-bol)
		       (save-excursion
			 (re-search-backward "^\\(Daily\\|Weekly\\) report"
					     nil t)
			 (point))))
      (end-of-line 0))))

(defun org-clock-get-table-data (file params)
  "Get the clocktable data for file FILE, with parameters PARAMS.
FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TIMESTAMP TIME)

LEVEL:     The level of the headline, as an integer.  This will be
           the reduced leve, so 1,2,3,... even if only odd levels
           are being used.
HEADLINE:  The text of the headline.  Depending on PARAMS, this may
           already be formatted like a link.
TIMESTAMP: If PARAMS require it, this will be a time stamp found in the
           entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
           in this sequence.
TIME:      The sum of all time spend in this tree, in minutes.  This time
           will of cause be restricted to the time block and tags match
           specified in PARAMS."
  (let* ((maxlevel (or (plist-get params :maxlevel) 3))
	 (timestamp (plist-get params :timestamp))
	 (ts (plist-get params :tstart))
	 (te (plist-get params :tend))
	 (block (plist-get params :block))
	 (link (plist-get params :link))
	 (tags (plist-get params :tags))
	 (properties (plist-get params :properties))
	 (inherit-property-p (plist-get params :inherit-props))
	 todo-only
	 (matcher (if tags (cdr (org-make-tags-matcher tags))))
	 cc range-text st p time level hdl props tsp tbl)

    (setq org-clock-file-total-minutes nil)
    (when block
      (setq cc (org-clock-special-range block nil t)
	    ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
    (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
    (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
    (when (and ts (listp ts))
      (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
    (when (and te (listp te))
      (setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
    ;; Now the times are strings we can parse.
    (if ts (setq ts (org-float-time
		     (apply 'encode-time (org-parse-time-string ts)))))
    (if te (setq te (org-float-time
		     (apply 'encode-time (org-parse-time-string te)))))
    (save-excursion
      (org-clock-sum ts te
		     (unless (null matcher)
		       (lambda ()
			 (let ((tags-list (org-get-tags-at)))
			   (eval matcher)))))
      (goto-char (point-min))
      (setq st t)
      (while (or (and (bobp) (prog1 st (setq st nil))
		      (get-text-property (point) :org-clock-minutes)
		      (setq p (point-min)))
		 (setq p (next-single-property-change
			  (point) :org-clock-minutes)))
	(goto-char p)
	(when (setq time (get-text-property p :org-clock-minutes))
	  (save-excursion
	    (beginning-of-line 1)
	    (when (and (looking-at (org-re "\\(\\*+\\)[ \t]+\\(.*?\\)\\([ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
		       (setq level (org-reduced-level
				    (- (match-end 1) (match-beginning 1))))
		       (<= level maxlevel))
	      (setq hdl (if (not link)
			    (match-string 2)
			  (org-make-link-string
			   (format "file:%s::%s"
				   (buffer-file-name)
				   (save-match-data
				     (org-make-org-heading-search-string
				      (match-string 2))))
			   (match-string 2)))
		    tsp (when timestamp
			  (setq props (org-entry-properties (point)))
			  (or (cdr (assoc "SCHEDULED" props))
			      (cdr (assoc "DEADLINE" props))
			      (cdr (assoc "TIMESTAMP" props))
			      (cdr (assoc "TIMESTAMP_IA" props))))
	    props (when properties
		    (remove nil
			    (mapcar
			     (lambda (p)
			       (when (org-entry-get (point) p inherit-property-p)
				 (cons p (org-entry-get (point) p inherit-property-p))))
			     properties))))
	      (when (> time 0) (push (list level hdl tsp time props) tbl))))))
      (setq tbl (nreverse tbl))
      (list file org-clock-file-total-minutes tbl))))

(defun org-clock-time% (total &rest strings)
  "Compute a time fraction in percent.
TOTAL s a time string like 10:21 specifying the total times.
STRINGS is a list of strings that should be checked for a time.
The first string that does have a time will be used.
This function is made for clock tables."
  (let ((re "\\([0-9]+\\):\\([0-9]+\\)")
	tot s)
    (save-match-data
      (catch 'exit
	(if (not (string-match re total))
	    (throw 'exit 0.)
	  (setq tot (+ (string-to-number (match-string 2 total))
		       (* 60 (string-to-number (match-string 1 total)))))
	  (if (= tot 0.) (throw 'exit 0.)))
	(while (setq s (pop strings))
	  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" s)
	      (throw 'exit
		     (/ (* 100.0 (+ (string-to-number (match-string 2 s))
				    (* 60 (string-to-number
					   (match-string 1 s)))))
			tot))))
	0))))

;; Saving and loading the clock

(defvar org-clock-loaded nil
  "Was the clock file loaded?")

(defun org-clock-save ()
  "Persist various clock-related data to disk.
The details of what will be saved are regulated by the variable
`org-clock-persist'."
  (when (and org-clock-persist
             (or org-clock-loaded
		 org-clock-has-been-used
		 (not (file-exists-p org-clock-persist-file))))
    (let (b)
      (with-current-buffer (find-file (expand-file-name org-clock-persist-file))
	(progn
	  (delete-region (point-min) (point-max))
	  ;;Store clock
	  (insert (format ";; org-persist.el - %s at %s\n"
			  system-name (format-time-string
				       (cdr org-time-stamp-formats))))
	  (if (and (memq org-clock-persist '(t clock))
		   (setq b (org-clocking-buffer))
		   (setq b (or (buffer-base-buffer b) b))
		   (buffer-live-p b)
		   (buffer-file-name b)
		   (or (not org-clock-persist-query-save)
		       (y-or-n-p (concat "Save current clock ("
					 (substring-no-properties
					  org-clock-heading)
					 ") "))))
	      (insert "(setq resume-clock '(\""
		      (buffer-file-name (org-clocking-buffer))
		      "\" . " (int-to-string (marker-position org-clock-marker))
		      "))\n"))
	  ;; Store clocked task history. Tasks are stored reversed to make
	  ;; reading simpler
	  (when (and (memq org-clock-persist '(t history))
		     org-clock-history)
	    (insert
	     "(setq stored-clock-history '("
	     (mapconcat
	      (lambda (m)
		(when (and (setq b (marker-buffer m))
			   (setq b (or (buffer-base-buffer b) b))
			   (buffer-live-p b)
			   (buffer-file-name b))
		  (concat "(\"" (buffer-file-name b)
			  "\" . " (int-to-string (marker-position m))
			  ")")))
	      (reverse org-clock-history) " ") "))\n"))
	  (save-buffer)
	  (kill-buffer (current-buffer)))))))

(defun org-clock-load ()
  "Load clock-related data from disk, maybe resuming a stored clock."
  (when (and org-clock-persist (not org-clock-loaded))
    (let ((filename (expand-file-name org-clock-persist-file))
	  (org-clock-in-resume 'auto-restart)
	  resume-clock stored-clock-history)
      (if (not (file-readable-p filename))
	  (message "Not restoring clock data; %s not found"
		   org-clock-persist-file)
	(message "%s" "Restoring clock data")
	(setq org-clock-loaded t)
	(load-file filename)
	;; load history
	(when stored-clock-history
	  (save-window-excursion
	    (mapc (lambda (task)
		    (if (file-exists-p (car task))
			(org-clock-history-push (cdr task)
						(find-file (car task)))))
		  stored-clock-history)))
	;; resume clock
	(when (and resume-clock org-clock-persist
		   (file-exists-p (car resume-clock))
		   (or (not org-clock-persist-query-resume)
		       (y-or-n-p
			(concat
			 "Resume clock ("
			 (with-current-buffer (find-file (car resume-clock))
			   (save-excursion
			     (goto-char (cdr resume-clock))
			     (org-back-to-heading t)
			     (and (looking-at org-complex-heading-regexp)
				  (match-string 4))))
			 ") "))))
	  (when (file-exists-p (car resume-clock))
	    (with-current-buffer (find-file (car resume-clock))
	      (goto-char (cdr resume-clock))
	      (let ((org-clock-auto-clock-resolution nil))
		(org-clock-in)
		(if (outline-invisible-p)
		    (org-show-context))))))))))

;;;###autoload
(defun org-clock-persistence-insinuate ()
  "Set up hooks for clock persistence."
  (add-hook 'org-mode-hook 'org-clock-load)
  (add-hook 'kill-emacs-hook 'org-clock-save))

;; Suggested bindings
(org-defkey org-mode-map "\C-c\C-x\C-e" 'org-clock-modify-effort-estimate)

(provide 'org-clock)

;;; org-clock.el ends here
