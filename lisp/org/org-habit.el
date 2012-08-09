;;; org-habit.el --- The habit tracking code for Org-mode

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw at gnu dot org>
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

;; This file contains the habit tracking code for Org-mode

;;; Code:

(require 'org)
(require 'org-agenda)

(eval-when-compile
  (require 'cl))

(defgroup org-habit nil
  "Options concerning habit tracking in Org-mode."
  :tag "Org Habit"
  :group 'org-progress)

(defcustom org-habit-graph-column 40
  "The absolute column at which to insert habit consistency graphs.
Note that consistency graphs will overwrite anything else in the buffer."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-preceding-days 21
  "Number of days before today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-show-habits t
  "If non-nil, show habits in agenda buffers."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-habits-only-for-today t
  "If non-nil, only show habits on today's agenda, and not for future days.
Note that even when shown for future days, the graph is always
relative to the current effective date."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-today-glyph ?!
  "Glyph character used to identify today."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defcustom org-habit-completed-glyph ?*
  "Glyph character used to show completed days on which a task was done."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defface org-habit-clear-face
  '((((background light)) (:background "#8270f9"))
    (((background dark)) (:background "blue")))
  "Face for days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-clear-future-face
  '((((background light)) (:background "#d6e4fc"))
    (((background dark)) (:background "midnightblue")))
  "Face for future days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-ready-face
  '((((background light)) (:background "#4df946"))
    (((background dark)) (:background "forestgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-ready-future-face
  '((((background light)) (:background "#acfca9"))
    (((background dark)) (:background "darkgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-alert-face
  '((((background light)) (:background "#f5f946"))
    (((background dark)) (:background "gold")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-alert-future-face
  '((((background light)) (:background "#fafca9"))
    (((background dark)) (:background "darkgoldenrod")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-overdue-face
  '((((background light)) (:background "#f9372d"))
    (((background dark)) (:background "firebrick")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-overdue-future-face
  '((((background light)) (:background "#fc9590"))
    (((background dark)) (:background "darkred")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)

(defun org-habit-duration-to-days (ts)
  (if (string-match "\\([0-9]+\\)\\([dwmy]\\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25))))))
    (error "Invalid duration string: %s" ts)))

(defun org-is-habit-p (&optional pom)
  "Is the task at POM or point a habit?"
  (string= "habit" (org-entry-get (or pom (point)) "STYLE")))

(defun org-habit-parse-todo (&optional pom)
  "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed

This list represents a \"habit\" for the rest of this module."
  (save-excursion
    (if pom (goto-char pom))
    (assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
	   (scheduled-repeat (org-get-repeat org-scheduled-string))
	   (end (org-entry-end-position))
	   (habit-entry (org-no-properties (nth 4 (org-heading-components))))
	   closed-dates deadline dr-days sr-days)
      (if scheduled
	  (setq scheduled (time-to-days scheduled))
	(error "Habit %s has no scheduled date" habit-entry))
      (unless scheduled-repeat
	(error
	 "Habit '%s' has no scheduled repeat period or has an incorrect one"
	 habit-entry))
      (setq sr-days (org-habit-duration-to-days scheduled-repeat))
      (unless (> sr-days 0)
	(error "Habit %s scheduled repeat period is less than 1d" habit-entry))
      (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	(setq dr-days (org-habit-duration-to-days
		       (match-string-no-properties 1 scheduled-repeat)))
	(if (<= dr-days sr-days)
	    (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
		   habit-entry scheduled-repeat))
	(setq deadline (+ scheduled (- dr-days sr-days))))
      (org-back-to-heading t)
      (let* ((maxdays (+ org-habit-preceding-days org-habit-following-days))
	     (reversed org-log-states-order-reversed)
	     (search (if reversed 're-search-forward 're-search-backward))
	     (limit (if reversed end (point)))
	     (count 0))
	(unless reversed (goto-char end))
	(while (and (< count maxdays)
		    (funcall search "- State \"DONE\".*\\[\\([^]]+\\)\\]" limit t))
	  (push (time-to-days
		 (org-time-string-to-time (match-string-no-properties 1)))
		closed-dates)
	  (setq count (1+ count))))
      (list scheduled sr-days deadline dr-days closed-dates))))

(defsubst org-habit-scheduled (habit)
  (nth 0 habit))
(defsubst org-habit-scheduled-repeat (habit)
  (nth 1 habit))
(defsubst org-habit-deadline (habit)
  (let ((deadline (nth 2 habit)))
    (or deadline
	(if (nth 3 habit)
	    (+ (org-habit-scheduled habit)
	       (1- (org-habit-scheduled-repeat habit)))
	  (org-habit-scheduled habit)))))
(defsubst org-habit-deadline-repeat (habit)
  (or (nth 3 habit)
      (org-habit-scheduled-repeat habit)))
(defsubst org-habit-done-dates (habit)
  (nth 4 habit))

(defsubst org-habit-get-priority (habit &optional moment)
  "Determine the relative priority of a habit.
This must take into account not just urgency, but consistency as well."
  (let ((pri 1000)
	(now (if moment (time-to-days moment) (org-today)))
	(scheduled (org-habit-scheduled habit))
	(deadline (org-habit-deadline habit)))
    ;; add 10 for every day past the scheduled date, and subtract for every
    ;; day before it
    (setq pri (+ pri (* (- now scheduled) 10)))
    ;; add 50 if the deadline is today
    (if (and (/= scheduled deadline)
	     (= now deadline))
	(setq pri (+ pri 50)))
    ;; add 100 for every day beyond the deadline date, and subtract 10 for
    ;; every day before it
    (let ((slip (- now (1- deadline))))
      (if (> slip 0)
	  (setq pri (+ pri (* slip 100)))
	(setq pri (+ pri (* slip 10)))))
    pri))

(defun org-habit-get-faces (habit &optional now-days scheduled-days donep)
  "Return faces for HABIT relative to NOW-DAYS and SCHEDULED-DAYS.
NOW-DAYS defaults to the current time's days-past-the-epoch if nil.
SCHEDULED-DAYS defaults to the habit's actual scheduled days if nil.

Habits are assigned colors on the following basis:
  Blue      Task is before the scheduled date.
  Green     Task is on or after scheduled date, but before the
	    end of the schedule's repeat period.
  Yellow    If the task has a deadline, then it is after schedule's
	    repeat period, but before the deadline.
  Orange    The task has reached the deadline day, or if there is
	    no deadline, the end of the schedule's repeat period.
  Red       The task has gone beyond the deadline day or the
	    schedule's repeat period."
  (let* ((scheduled (or scheduled-days (org-habit-scheduled habit)))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (scheduled-end (+ scheduled (1- s-repeat)))
	 (d-repeat (org-habit-deadline-repeat habit))
	 (deadline (if scheduled-days
		       (+ scheduled-days (- d-repeat s-repeat))
		     (org-habit-deadline habit)))
	 (m-days (or now-days (time-to-days (current-time)))))
    (cond
     ((< m-days scheduled)
      '(org-habit-clear-face . org-habit-clear-future-face))
     ((< m-days deadline)
      '(org-habit-ready-face . org-habit-ready-future-face))
     ((= m-days deadline)
      (if donep
	  '(org-habit-ready-face . org-habit-ready-future-face)
	'(org-habit-alert-face . org-habit-alert-future-face)))
     (t
      '(org-habit-overdue-face . org-habit-overdue-future-face)))))

(defun org-habit-build-graph (habit starting current ending)
  "Build a graph for the given HABIT, from STARTING to ENDING.
CURRENT gives the current time between STARTING and ENDING, for
the purpose of drawing the graph.  It need not be the actual
current time."
  (let* ((done-dates (sort (org-habit-done-dates habit) '<))
	 (scheduled (org-habit-scheduled habit))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (start (time-to-days starting))
	 (now (time-to-days current))
	 (end (time-to-days ending))
	 (graph (make-string (1+ (- end start)) ?\ ))
	 (index 0)
	 last-done-date)
    (while (and done-dates (< (car done-dates) start))
      (setq last-done-date (car done-dates)
	    done-dates (cdr done-dates)))
    (while (< start end)
      (let* ((in-the-past-p (< start now))
	     (todayp (= start now))
	     (donep (and done-dates
			 (= start (car done-dates))))
	     (faces (if (and in-the-past-p
			     (not last-done-date)
			     (not (< scheduled now)))
			'(org-habit-clear-face . org-habit-clear-future-face)
		      (org-habit-get-faces
		       habit start (and in-the-past-p
					(if last-done-date
					    (+ last-done-date s-repeat)
					  scheduled))
		       donep)))
	     markedp face)
	(if donep
	    (let ((done-time (time-add
			      starting
			      (days-to-time
			       (- start (time-to-days starting))))))

	      (aset graph index org-habit-completed-glyph)
	      (setq markedp t)
	      (put-text-property
	       index (1+ index) 'help-echo
	       (format-time-string (org-time-stamp-format) done-time) graph)
	      (while (and done-dates
			  (= start (car done-dates)))
		(setq last-done-date (car done-dates)
		      done-dates (cdr done-dates))))
	  (if todayp
	      (aset graph index org-habit-today-glyph)))
	(setq face (if (or in-the-past-p todayp)
		       (car faces)
		     (cdr faces)))
	(if (and in-the-past-p
		 (not (eq face 'org-habit-overdue-face))
		 (not markedp))
	    (setq face (cdr faces)))
	(put-text-property index (1+ index) 'face face graph))
      (setq start (1+ start)
	    index (1+ index)))
    graph))

(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t) l c
	(buffer-invisibility-spec '(org-link))
	(moment (time-subtract (current-time)
			       (list 0 (* 3600 org-extend-today-until) 0)))
	disabled-overlays)
    ;; Disable filters; this helps with alignment if there are links.
    (mapc (lambda (ol)
	    (when (overlay-get ol 'invisible)
	      (overlay-put ol 'invisible nil)
	      (setq disabled-overlays (cons ol disabled-overlays))))
	  (overlays-in (point-min) (point-max)))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p)))
	  (when habit
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			      (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-habit-build-graph
	      habit
	      (time-subtract moment (days-to-time org-habit-preceding-days))
	      moment
	      (time-add moment (days-to-time org-habit-following-days))))))
	(forward-line)))
    (mapc (lambda (ol) (overlay-put ol 'invisible t))
	  disabled-overlays)))

(defun org-habit-toggle-habits ()
  "Toggle display of habits in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-habit-show-habits (not org-habit-show-habits))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Habits turned %s"
	   (if org-habit-show-habits "on" "off")))

(org-defkey org-agenda-mode-map "K" 'org-habit-toggle-habits)

(provide 'org-habit)

;;; org-habit.el ends here
