;;; org-icalendar.el --- iCalendar export for Org-mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

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

;;; Code:

(require 'org-exp)

(eval-when-compile
  (require 'cl))

(declare-function org-bbdb-anniv-export-ical "org-bbdb" nil)

(defgroup org-export-icalendar nil
  "Options specific for iCalendar export of Org-mode files."
  :tag "Org Export iCalendar"
  :group 'org-export)

(defcustom org-combined-agenda-icalendar-file "~/org.ics"
  "The file name for the iCalendar file covering all agenda files.
This file is created with the command \\[org-export-icalendar-all-agenda-files].
The file name should be absolute, the file will be overwritten without warning."
  :group 'org-export-icalendar
  :type 'file)

(defcustom org-icalendar-alarm-time 0
  "Number of minutes for triggering an alarm for exported timed events.
A zero value (the default) turns off the definition of an alarm trigger
for timed events.  If non-zero, alarms are created.

- a single alarm per entry is defined
- The alarm will go off N minutes before the event
- only a DISPLAY action is defined."
  :group 'org-export-icalendar
  :version "24.1"
  :type 'integer)

(defcustom org-icalendar-combined-name "OrgMode"
  "Calendar name for the combined iCalendar representing all agenda files."
  :group 'org-export-icalendar
  :type 'string)

(defcustom org-icalendar-combined-description nil
  "Calendar description for the combined iCalendar (all agenda files)."
  :group 'org-export-icalendar
  :version "24.1"
  :type 'string)

(defcustom org-icalendar-use-plain-timestamp t
  "Non-nil means make an event from every plain time stamp."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-honor-noexport-tag nil
  "Non-nil means don't export entries with a tag in `org-export-exclude-tags'."
  :group 'org-export-icalendar
  :version "24.1"
  :type 'boolean)

(defcustom org-icalendar-use-deadline '(event-if-not-todo todo-due)
  "Contexts where iCalendar export should use a deadline time stamp.
This is a list with several symbols in it.  Valid symbol are:

event-if-todo       Deadlines in TODO entries become calendar events.
event-if-not-todo   Deadlines in non-TODO entries become calendar events.
todo-due            Use deadlines in TODO entries as due-dates"
  :group 'org-export-icalendar
  :type '(set :greedy t
	      (const :tag "Deadlines in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "Deadline in TODO entries become events"
		     event-if-todo)
	      (const :tag "Deadlines in TODO entries become due-dates"
		     todo-due)))

(defcustom org-icalendar-use-scheduled '(todo-start)
  "Contexts where iCalendar export should use a scheduling time stamp.
This is a list with several symbols in it.  Valid symbol are:

event-if-todo       Scheduling time stamps in TODO entries become an event.
event-if-not-todo   Scheduling time stamps in non-TODO entries become an event.
todo-start          Scheduling time stamps in TODO entries become start date.
                    Some calendar applications show TODO entries only after
                    that date."
  :group 'org-export-icalendar
  :type '(set :greedy t
	      (const :tag
		     "SCHEDULED timestamps in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "SCHEDULED timestamps in TODO entries become events"
		     event-if-todo)
	      (const :tag "SCHEDULED in TODO entries become start date"
		     todo-start)))

(defcustom org-icalendar-categories '(local-tags category)
  "Items that should be entered into the categories field.
This is a list of symbols, the following are valid:

category    The Org-mode category of the current file or tree
todo-state  The todo state, if any
local-tags  The tags, defined in the current line
all-tags    All tags, including inherited ones."
  :group 'org-export-icalendar
  :type '(repeat
	  (choice
	   (const :tag "The file or tree category" category)
	   (const :tag "The TODO state" todo-state)
	   (const :tag "Tags defined in current line" local-tags)
	   (const :tag "All tags, including inherited ones" all-tags))))

(defcustom org-icalendar-include-todo nil
  "Non-nil means export to iCalendar files should also cover TODO items.
Valid values are:
nil         don't include any TODO items
t           include all TODO items that are not in a DONE state
unblocked   include all TODO items that are not blocked
all         include both done and not done items."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "None" nil)
	  (const :tag "Unfinished" t)
	  (const :tag "Unblocked" unblocked)
	  (const :tag "All" all)))

(defvar org-icalendar-verify-function nil
  "Function to verify entries for iCalendar export.
This can be set to a function that will be called at each entry that
is considered for export to iCalendar.  When the function returns nil,
the entry will be skipped.  When it returns a non-nil value, the entry
will be considered for export.
This is used internally when an agenda buffer is exported to an ics file,
to make sure that only entries currently listed in the agenda will end
up in the ics file.  But for normal iCalendar export, you can use this
for whatever you need.")

(defcustom org-icalendar-include-bbdb-anniversaries nil
  "Non-nil means a combined iCalendar files should include anniversaries.
The anniversaries are define in the BBDB database."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-include-sexps t
  "Non-nil means export to iCalendar files should also cover sexp entries.
These are entries like in the diary, but directly in an Org-mode file."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-include-body 100
  "Amount of text below headline to be included in iCalendar export.
This is a number of characters that should maximally be included.
Properties, scheduling and clocking lines will always be removed.
The text will be inserted into the DESCRIPTION field."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "Nothing" nil)
	  (const :tag "Everything" t)
	  (integer :tag "Max characters")))

(defcustom org-icalendar-store-UID nil
  "Non-nil means store any created UIDs in properties.
The iCalendar standard requires that all entries have a unique identifier.
Org will create these identifiers as needed.  When this variable is non-nil,
the created UIDs will be stored in the ID property of the entry.  Then the
next time this entry is exported, it will be exported with the same UID,
superseding the previous form of it.  This is essential for
synchronization services.
This variable is not turned on by default because we want to avoid creating
a property drawer in every entry if people are only playing with this feature,
or if they are only using it locally."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-timezone (getenv "TZ")
  "The time zone string for iCalendar export.
When nil of the empty string, use the abbreviation retrieved from Emacs."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "Unspecified" nil)
	  (string :tag "Time zone")))

;; Backward compatibility with previous variable
(defvar org-icalendar-use-UTC-date-time nil)
(defcustom org-icalendar-date-time-format
  (if org-icalendar-use-UTC-date-time
      ":%Y%m%dT%H%M%SZ"
    ":%Y%m%dT%H%M%S")
  "Format-string for exporting icalendar DATE-TIME.
See `format-time-string' for a full documentation.  The only
difference is that `org-icalendar-timezone' is used for %Z.

Interesting value are:
 - \":%Y%m%dT%H%M%S\" for local time
 - \";TZID=%Z:%Y%m%dT%H%M%S\" for local time with explicit timezone
 - \":%Y%m%dT%H%M%SZ\" for time expressed in Universal Time"

  :group 'org-export-icalendar
  :version "24.1"
  :type '(choice
	  (const :tag "Local time" ":%Y%m%dT%H%M%S")
	  (const :tag "Explicit local time" ";TZID=%Z:%Y%m%dT%H%M%S")
	  (const :tag "Universal time" ":%Y%m%dT%H%M%SZ")
	  (string :tag "Explicit format")))

(defun org-icalendar-use-UTC-date-timep ()
  (char-equal (elt org-icalendar-date-time-format
		   (1- (length org-icalendar-date-time-format))) ?Z))

;;; iCalendar export

;;;###autoload
(defun org-export-icalendar-this-file ()
  "Export current file as an iCalendar file.
The iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'."
  (interactive)
  (org-export-icalendar nil buffer-file-name))

;;;###autoload
(defun org-export-icalendar-all-agenda-files ()
  "Export all files in the variable `org-agenda-files' to iCalendar .ics files.
Each iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'."
  (interactive)
  (apply 'org-export-icalendar nil (org-agenda-files t)))

;;;###autoload
(defun org-export-icalendar-combine-agenda-files ()
  "Export all files in `org-agenda-files' to a single combined iCalendar file.
The file is stored under the name `org-combined-agenda-icalendar-file'."
  (interactive)
  (apply 'org-export-icalendar t (org-agenda-files t)))

(defun org-export-icalendar (combine &rest files)
  "Create iCalendar files for all elements of FILES.
If COMBINE is non-nil, combine all calendar entries into a single large
file and store it under the name `org-combined-agenda-icalendar-file'."
  (save-excursion
    (org-prepare-agenda-buffers files)
    (let* ((dir (org-export-directory
		 :ical (list :publishing-directory
			     org-export-publishing-directory)))
	   file ical-file ical-buffer category started org-agenda-new-buffers)
      (and (get-buffer "*ical-tmp*") (kill-buffer "*ical-tmp*"))
      (when combine
	(setq ical-file
	      (if (file-name-absolute-p org-combined-agenda-icalendar-file)
		  org-combined-agenda-icalendar-file
		(expand-file-name org-combined-agenda-icalendar-file dir))
	      ical-buffer (org-get-agenda-file-buffer ical-file))
	(set-buffer ical-buffer) (erase-buffer))
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (set-buffer (org-get-agenda-file-buffer file))
	  (unless combine
	    (setq ical-file (concat (file-name-as-directory dir)
				    (file-name-sans-extension
				     (file-name-nondirectory buffer-file-name))
				    ".ics"))
	    (setq ical-buffer (org-get-agenda-file-buffer ical-file))
	    (with-current-buffer ical-buffer (erase-buffer)))
	  (setq category (or org-category
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))))
	  (if (symbolp category) (setq category (symbol-name category)))
	  (let ((standard-output ical-buffer))
	    (if combine
		(and (not started) (setq started t)
		     (org-start-icalendar-file org-icalendar-combined-name))
	      (org-start-icalendar-file category))
	    (org-print-icalendar-entries combine)
	    (when (or (and combine (not files)) (not combine))
	      (when (and combine org-icalendar-include-bbdb-anniversaries)
		(require 'org-bbdb)
		(org-bbdb-anniv-export-ical))
	      (org-finish-icalendar-file)
	      (set-buffer ical-buffer)
	      (run-hooks 'org-before-save-iCalendar-file-hook)
	      (save-buffer)
	      (run-hooks 'org-after-save-iCalendar-file-hook)
	      (and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))
	      ))))
      (org-release-buffers org-agenda-new-buffers))))

(defvar org-before-save-iCalendar-file-hook nil
  "Hook run before  an iCalendar file has been saved.
This can be used to modify the result of the export.")

(defvar org-after-save-iCalendar-file-hook nil
  "Hook run after an iCalendar file has been saved.
The iCalendar buffer is still current when this hook is run.
A good way to use this is to tell a desktop calendar application to re-read
the iCalendar file.")

(defvar org-agenda-default-appointment-duration) ; defined in org-agenda.el
(defun org-print-icalendar-entries (&optional combine)
  "Print iCalendar entries for the current Org-mode file to `standard-output'.
When COMBINE is non nil, add the category to each line."
  (require 'org-agenda)
  (let ((re1 (concat org-ts-regexp "\\|<%%([^>\n]+>"))
	(re2 (concat "--?-?\\(" org-ts-regexp "\\)"))
	(dts (org-ical-ts-to-string
	      (format-time-string (cdr org-time-stamp-formats) (current-time))
	      "DTSTART"))
	hd ts ts2 state status (inc t) pos b sexp rrule
	scheduledp deadlinep todo prefix due start tags
	tmp pri categories location summary desc uid alarm
	(sexp-buffer (get-buffer-create "*ical-tmp*")))
    (org-refresh-category-properties)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re1 nil t)
	(catch :skip
	  (org-agenda-skip)
	  (when org-icalendar-verify-function
	    (unless (save-match-data (funcall org-icalendar-verify-function))
	      (outline-next-heading)
	      (backward-char 1)
	      (throw :skip nil)))
	  (setq pos (match-beginning 0)
		ts (match-string 0)
		tags (org-get-tags-at)
		inc t
		hd (condition-case nil
		       (org-icalendar-cleanup-string
			(org-get-heading t))
		     (error (throw :skip nil)))
		summary (org-icalendar-cleanup-string
			 (org-entry-get nil "SUMMARY"))
		desc (org-icalendar-cleanup-string
		      (or (org-entry-get nil "DESCRIPTION")
			  (and org-icalendar-include-body (org-get-entry)))
		      t org-icalendar-include-body)
		location (org-icalendar-cleanup-string
			  (org-entry-get nil "LOCATION" 'selective))
		uid (if org-icalendar-store-UID
			(org-id-get-create)
		      (or (org-id-get) (org-id-new)))
		categories (org-export-get-categories)
		alarm ""
		deadlinep nil scheduledp nil)
	  (if (looking-at re2)
	      (progn
		(goto-char (match-end 0))
		(setq ts2 (match-string 1)
		      inc (not (string-match "[0-9]\\{1,2\\}:[0-9][0-9]" ts2))))
	    (setq tmp (buffer-substring (max (point-min)
					     (- pos org-ds-keyword-length))
					pos)
		  ts2 (if (string-match "[0-9]\\{1,2\\}:[0-9][0-9]-\\([0-9]\\{1,2\\}:[0-9][0-9]\\)" ts)
			  (progn
			    (setq inc nil)
			    (replace-match "\\1" t nil ts))
			ts)
		  deadlinep (string-match org-deadline-regexp tmp)
		  scheduledp (string-match org-scheduled-regexp tmp)
		  todo (org-get-todo-state)
		  ;; donep (org-entry-is-done-p)
		  ))
	  (when (and (not org-icalendar-use-plain-timestamp)
		     (not deadlinep) (not scheduledp))
	    (throw :skip t))
	  ;; don't export entries with a :noexport: tag
	  (when (and org-icalendar-honor-noexport-tag
		     (delq nil (mapcar (lambda(x)
					 (member x org-export-exclude-tags)) tags)))
	    (throw :skip t))
	  (when (and
		 deadlinep
		 (if todo
		     (not (memq 'event-if-todo org-icalendar-use-deadline))
		   (not (memq 'event-if-not-todo org-icalendar-use-deadline))))
	    (throw :skip t))
	  (when (and
		 scheduledp
		 (if todo
		     (not (memq 'event-if-todo org-icalendar-use-scheduled))
		   (not (memq 'event-if-not-todo org-icalendar-use-scheduled))))
	    (throw :skip t))
	  (setq prefix (if deadlinep "DL-" (if scheduledp "SC-" "TS-")))
	  (if (or (string-match org-tr-regexp hd)
		  (string-match org-ts-regexp hd))
	      (setq hd (replace-match "" t t hd)))
	  (if (string-match "\\+\\([0-9]+\\)\\([dwmy]\\)>" ts)
	      (setq rrule
		    (concat "\nRRULE:FREQ="
			    (cdr (assoc
				  (match-string 2 ts)
				  '(("d" . "DAILY")("w" . "WEEKLY")
				    ("m" . "MONTHLY")("y" . "YEARLY"))))
			    ";INTERVAL=" (match-string 1 ts)))
	    (setq rrule ""))
	  (setq summary (or summary hd))
	  ;; create an alarm entry if the entry is timed.  this is not very general in that:
	  ;; (a) only one alarm per entry is defined,
	  ;; (b) only minutes are allowed for the trigger period ahead of the start time, and
	  ;; (c) only a DISPLAY action is defined.
	  ;; [ESF]
	  (let ((t1 (ignore-errors (org-parse-time-string ts 'nodefault))))
	    (if (and (> org-icalendar-alarm-time 0)
		     (car t1) (nth 1 t1) (nth 2 t1))
		(setq alarm (format "\nBEGIN:VALARM\nACTION:DISPLAY\nDESCRIPTION:%s\nTRIGGER:-P0DT0H%dM0S\nEND:VALARM" summary org-icalendar-alarm-time))
	      (setq alarm ""))
	    )
	  (if (string-match org-bracket-link-regexp summary)
	      (setq summary
		    (replace-match (if (match-end 3)
				       (match-string 3 summary)
				     (match-string 1 summary))
				   t t summary)))
	  (if deadlinep (setq summary (concat "DL: " summary)))
	  (if scheduledp (setq summary (concat "S: " summary)))
	  (if (string-match "\\`<%%" ts)
	      (with-current-buffer sexp-buffer
		(let ((entry (substring ts 1 -1)))
		  (put-text-property 0 1 'uid
				     (concat " " prefix uid) entry)
		  (insert entry " " summary "\n")))
	    (princ (format "BEGIN:VEVENT
UID: %s
%s
%s%s
SUMMARY:%s%s%s
CATEGORIES:%s%s
END:VEVENT\n"
			   (concat prefix uid)
			   (org-ical-ts-to-string ts "DTSTART")
			   (org-ical-ts-to-string ts2 "DTEND" inc)
			   rrule summary
			   (if (and desc (string-match "\\S-" desc))
			       (concat "\nDESCRIPTION: " desc) "")
			   (if (and location (string-match "\\S-" location))
			       (concat "\nLOCATION: " location) "")
			   categories
			   alarm)))))
      (when (and org-icalendar-include-sexps
		 (condition-case nil (require 'icalendar) (error nil))
		 (fboundp 'icalendar-export-region))
	;; Get all the literal sexps
	(goto-char (point-min))
	(while (re-search-forward "^&?%%(" nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (when org-icalendar-verify-function
	      (unless (save-match-data (funcall org-icalendar-verify-function))
		(outline-next-heading)
		(backward-char 1)
		(throw :skip nil)))
	    (setq b (match-beginning 0))
	    (goto-char (1- (match-end 0)))
	    (forward-sexp 1)
	    (end-of-line 1)
	    (setq sexp (buffer-substring b (point)))
	    (with-current-buffer sexp-buffer
	      (insert sexp "\n"))))
	(princ (org-diary-to-ical-string sexp-buffer))
	(kill-buffer sexp-buffer))

      (when org-icalendar-include-todo
	(setq prefix "TODO-")
	(goto-char (point-min))
	(while (re-search-forward org-complex-heading-regexp nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (when org-icalendar-verify-function
	      (unless (save-match-data
			(funcall org-icalendar-verify-function))
		(outline-next-heading)
		(backward-char 1)
		(throw :skip nil)))
	    (setq state (match-string 2))
	    (setq status (if (member state org-done-keywords)
			     "COMPLETED" "NEEDS-ACTION"))
	    (when (and state
		       (cond
			;; check if the state is one we should use
			((eq org-icalendar-include-todo 'all)
			 ;; all should be included
			 t)
			((eq org-icalendar-include-todo 'unblocked)
			 ;; only undone entries that are not blocked
			 (and (member state org-not-done-keywords)
			      (or (not org-blocker-hook)
				  (save-match-data
				    (run-hook-with-args-until-failure
				     'org-blocker-hook
				     (list :type 'todo-state-change
					   :position (point-at-bol)
					   :from 'todo
					   :to 'done))))))
			((eq org-icalendar-include-todo t)
			 ;; include everything that is not done
			 (member state org-not-done-keywords))))
	      (setq hd (match-string 4)
		    summary (org-icalendar-cleanup-string
			     (org-entry-get nil "SUMMARY"))
		    desc (org-icalendar-cleanup-string
			  (or (org-entry-get nil "DESCRIPTION")
			      (and org-icalendar-include-body (org-get-entry)))
			  t org-icalendar-include-body)
		    location (org-icalendar-cleanup-string
			      (org-entry-get nil "LOCATION" 'selective))
		    due (and (member 'todo-due org-icalendar-use-deadline)
			     (org-entry-get nil "DEADLINE"))
		    start (and (member 'todo-start org-icalendar-use-scheduled)
			     (org-entry-get nil "SCHEDULED"))
		    categories (org-export-get-categories)
		    uid (if org-icalendar-store-UID
			    (org-id-get-create)
			  (or (org-id-get) (org-id-new))))
	      (and due (setq due (org-ical-ts-to-string due "DUE")))
	      (and start (setq start (org-ical-ts-to-string start "DTSTART")))

	      (if (string-match org-bracket-link-regexp hd)
		  (setq hd (replace-match (if (match-end 3) (match-string 3 hd)
					    (match-string 1 hd))
					  t t hd)))
	      (if (string-match org-priority-regexp hd)
		  (setq pri (string-to-char (match-string 2 hd))
			hd (concat (substring hd 0 (match-beginning 1))
				   (substring hd (match-end 1))))
		(setq pri org-default-priority))
	      (setq pri (floor (- 9 (* 8. (/ (float (- org-lowest-priority pri))
					     (- org-lowest-priority org-highest-priority))))))

	      (princ (format "BEGIN:VTODO
UID: %s
%s
SUMMARY:%s%s%s%s
CATEGORIES:%s
SEQUENCE:1
PRIORITY:%d
STATUS:%s
END:VTODO\n"
			     (concat prefix uid)
			     (or start dts)
			     (or summary hd)
			     (if (and location (string-match "\\S-" location))
				 (concat "\nLOCATION: " location) "")
			     (if (and desc (string-match "\\S-" desc))
				 (concat "\nDESCRIPTION: " desc) "")
			     (if due (concat "\n" due) "")
			     categories
			     pri status)))))))))

(defun org-export-get-categories ()
  "Get categories according to `org-icalendar-categories'."
  (let ((cs org-icalendar-categories) c rtn tmp)
    (while (setq c (pop cs))
      (cond
       ((eq c 'category) (push (org-get-category) rtn))
       ((eq c 'todo-state)
	(setq tmp (org-get-todo-state))
	(and tmp (push tmp rtn)))
       ((eq c 'local-tags)
	(setq rtn (append (nreverse (org-get-local-tags-at (point))) rtn)))
       ((eq c 'all-tags)
	(setq rtn (append (nreverse (org-get-tags-at (point))) rtn)))))
    (mapconcat 'identity (nreverse rtn) ",")))

(defun org-icalendar-cleanup-string (s &optional is-body maxlength)
  "Take out stuff and quote what needs to be quoted.
When IS-BODY is non-nil, assume that this is the body of an item, clean up
whitespace, newlines, drawers, and timestamps, and cut it down to MAXLENGTH
characters."
  (if (not s)
      nil
    (if is-body
      (let ((re (concat "\\(" org-drawer-regexp "\\)[^\000]*?:END:.*\n?"))
	    (re2 (concat "^[ \t]*" org-keyword-time-regexp ".*\n?")))
	(while (string-match re s) (setq s (replace-match "" t t s)))
	(while (string-match re2 s) (setq s (replace-match "" t t s))))
      (setq s (replace-regexp-in-string "[[:space:]]+" " " s)))
    (let ((start 0))
      (while (string-match "\\([,;]\\)" s start)
	(setq start (+ (match-beginning 0) 2)
	      s (replace-match "\\\\\\1" nil nil s))))
    (setq s (org-trim s))
    (when is-body
      (while (string-match "[ \t]*\n[ \t]*" s)
	(setq s (replace-match "\\n" t t s))))
    (if is-body
	(if maxlength
	    (if (and (numberp maxlength)
		     (> (length s) maxlength))
		(setq s (substring s 0 maxlength)))))
    s))

(defun org-icalendar-cleanup-string-rfc2455 (s &optional is-body maxlength)
  "Take out stuff and quote what needs to be quoted.
When IS-BODY is non-nil, assume that this is the body of an item, clean up
whitespace, newlines, drawers, and timestamps, and cut it down to MAXLENGTH
characters.
This seems to be more like RFC 2455, but it causes problems, so it is
not used right now."
  (if (not s)
      nil
    (if is-body
	(let ((re (concat "\\(" org-drawer-regexp "\\)[^\000]*?:END:.*\n?"))
	      (re2 (concat "^[ \t]*" org-keyword-time-regexp ".*\n?")))
	  (while (string-match re s) (setq s (replace-match "" t t s)))
	  (while (string-match re2 s) (setq s (replace-match "" t t s)))
	  (setq s (org-trim s))
	  (while (string-match "[ \t]*\n[ \t]*" s)
	    (setq s (replace-match "\\n" t t s)))
	  (if maxlength
	      (if (and (numberp maxlength)
		       (> (length s) maxlength))
		  (setq s (substring s 0 maxlength)))))
      (setq s (org-trim s)))
    (while (string-match "\"" s) (setq s (replace-match "''" t t s)))
    (when (string-match "[;,:]" s) (setq s (concat "\"" s "\"")))
    s))

(defun org-start-icalendar-file (name)
  "Start an iCalendar file by inserting the header."
  (let ((user user-full-name)
	(name (or name "unknown"))
	(timezone (if (> (length org-icalendar-timezone) 0)
		      org-icalendar-timezone
		    (cadr (current-time-zone))))
	(description org-icalendar-combined-description))
    (princ
     (format "BEGIN:VCALENDAR
VERSION:2.0
X-WR-CALNAME:%s
PRODID:-//%s//Emacs with Org-mode//EN
X-WR-TIMEZONE:%s
X-WR-CALDESC:%s
CALSCALE:GREGORIAN\n" name user timezone description))))

(defun org-finish-icalendar-file ()
  "Finish an iCalendar file by inserting the END statement."
  (princ "END:VCALENDAR\n"))

(defun org-ical-ts-to-string (s keyword &optional inc)
  "Take a time string S and convert it to iCalendar format.
KEYWORD is added in front, to make a complete line like DTSTART....
When INC is non-nil, increase the hour by two (if time string contains
a time), or the day by one (if it does not contain a time)."
  (let ((t1 (ignore-errors (org-parse-time-string s 'nodefault)))
	t2 fmt have-time time)
    (if (not t1)
	""
      (if (and (car t1) (nth 1 t1) (nth 2 t1))
	  (setq t2 t1 have-time t)
	(setq t2 (org-parse-time-string s)))
      (let ((s (car t2))   (mi (nth 1 t2)) (h (nth 2 t2))
	    (d (nth 3 t2)) (m  (nth 4 t2)) (y (nth 5 t2)))
	(when inc
	  (if have-time
	      (if org-agenda-default-appointment-duration
		  (setq mi (+ org-agenda-default-appointment-duration mi))
		(setq h (+ 2 h)))
	    (setq d (1+ d))))
	(setq time (encode-time s mi h d m y)))
      (setq fmt (if have-time
		    (replace-regexp-in-string "%Z"
					      org-icalendar-timezone
					      org-icalendar-date-time-format)
		    ";VALUE=DATE:%Y%m%d"))
      (concat keyword (format-time-string fmt time
					  (and (org-icalendar-use-UTC-date-timep)
					       have-time))))))

(provide 'org-icalendar)

;;; org-icalendar.el ends here
