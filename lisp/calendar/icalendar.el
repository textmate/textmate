;;; icalendar.el --- iCalendar implementation -*-coding: utf-8 -*-

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author:         Ulf Jasper <ulf.jasper@web.de>
;; Created:        August 2002
;; Keywords:       calendar
;; Human-Keywords: calendar, diary, iCalendar, vCalendar
;; Version:        0.19

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

;; This package is documented in the Emacs Manual.

;;   Please note:
;; - Diary entries which have a start time but no end time are assumed to
;;   last for one hour when they are exported.
;; - Weekly diary entries are assumed to occur the first time in the first
;;   week of the year 2000 when they are exported.
;; - Yearly diary entries are assumed to occur the first time in the year
;;   1900 when they are exported.
;; - Float diary entries are assumed to occur the first time on the
;;   day when they are exported.

;;; History:

;;  0.07 onwards: see lisp/ChangeLog

;;  0.06:  (2004-10-06)
;;  - Bugfixes regarding icalendar-import-format-*.
;;  - Fix in icalendar-convert-diary-to-ical -- thanks to Philipp Grau.

;;  0.05: (2003-06-19)
;;  - New import format scheme: Replaced icalendar-import-prefix-*,
;;    icalendar-import-ignored-properties, and
;;    icalendar-import-separator with icalendar-import-format(-*).
;;  - icalendar-import-file and icalendar-convert-diary-to-ical
;;    have an extra parameter which should prevent them from
;;    erasing their target files (untested!).
;;  - Tested with Emacs 21.3.2

;;  0.04:
;;  - Bugfix: import: double quoted param values did not work
;;  - Read DURATION property when importing.
;;  - Added parameter icalendar-duration-correction.

;;  0.03: (2003-05-07)
;;  - Export takes care of european-calendar-style.
;;  - Tested with Emacs 21.3.2 and XEmacs 21.4.12

;;  0.02:
;;  - Should work in XEmacs now.  Thanks to Len Trigg for the XEmacs patches!
;;  - Added exporting from Emacs diary to ical.
;;  - Some bugfixes, after testing with calendars from http://icalshare.com.
;;  - Tested with Emacs 21.3.2 and XEmacs 21.4.12

;;  0.01: (2003-03-21)
;;  - First published version.  Trial version.  Alpha version.

;; ======================================================================
;; To Do:

;;  * Import from ical to diary:
;;    + Need more properties for icalendar-import-format
;;      (added all that Mozilla Calendar uses)
;;      From iCal specifications (RFC2445: 4.8.1), icalendar.el lacks
;;      ATTACH, CATEGORIES, COMMENT, GEO, PERCENT-COMPLETE (VTODO),
;;      PRIORITY, RESOURCES) not considering date/time and time-zone
;;    + check vcalendar version
;;    + check (unknown) elements
;;    + recurring events!
;;    + works for european style calendars only! Does it?
;;    + alarm
;;    + exceptions in recurring events
;;    + the parser is too soft
;;    + error log is incomplete
;;    + nice to have: #include "webcal://foo.com/some-calendar.ics"
;;    + timezones probably still need some improvements.

;;  * Export from diary to ical
;;    + diary-date, diary-float, and self-made sexp entries are not
;;      understood

;;  * Other things
;;    + clean up all those date/time parsing functions
;;    + Handle todo items?
;;    + Check iso 8601 for datetime and period
;;    + Which chars to (un)escape?


;;; Code:

(defconst icalendar-version "0.19"
  "Version number of icalendar.el.")

;; ======================================================================
;; Customizables
;; ======================================================================
(defgroup icalendar nil
  "iCalendar support."
  :prefix "icalendar-"
  :group 'calendar)

(defcustom icalendar-import-format
  "%s%d%l%o"
  "Format for importing events from iCalendar into Emacs diary.
It defines how iCalendar events are inserted into diary file.
This may either be a string or a function.

In case of a formatting STRING the following specifiers can be used:
%c Class, see `icalendar-import-format-class'
%d Description, see `icalendar-import-format-description'
%l Location, see `icalendar-import-format-location'
%o Organizer, see `icalendar-import-format-organizer'
%s Summary, see `icalendar-import-format-summary'
%t Status, see `icalendar-import-format-status'
%u URL, see `icalendar-import-format-url'

A formatting FUNCTION will be called with a VEVENT as its only
argument.  It must return a string.  See
`icalendar-import-format-sample' for an example."
  :type '(choice
          (string :tag "String")
          (function :tag "Function"))
  :group 'icalendar)

(defcustom icalendar-import-format-summary
  "%s"
  "Format string defining how the summary element is formatted.
This applies only if the summary is not empty! `%s' is replaced
by the summary."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-description
  "\n Desc: %s"
  "Format string defining how the description element is formatted.
This applies only if the description is not empty! `%s' is
replaced by the description."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-location
  "\n Location: %s"
  "Format string defining how the location element is formatted.
This applies only if the location is not empty! `%s' is replaced
by the location."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-organizer
  "\n Organizer: %s"
  "Format string defining how the organizer element is formatted.
This applies only if the organizer is not empty! `%s' is
replaced by the organizer."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-url
  "\n URL: %s"
  "Format string defining how the URL element is formatted.
This applies only if the URL is not empty! `%s' is replaced by
the URL."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-status
  "\n Status: %s"
  "Format string defining how the status element is formatted.
This applies only if the status is not empty! `%s' is replaced by
the status."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-class
  "\n Class: %s"
  "Format string defining how the class element is formatted.
This applies only if the class is not empty! `%s' is replaced by
the class."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-recurring-start-year
  2005
  "Start year for recurring events.
Some calendar browsers only propagate recurring events for
several years beyond the start time.  Set this string to a year
just before the start of your personal calendar."
  :type 'integer
  :group 'icalendar)

(defcustom icalendar-export-hidden-diary-entries
  t
  "Determines whether hidden diary entries are exported.
If non-nil hidden diary entries (starting with `&') get exported,
if nil they are ignored."
  :type 'boolean
  :group 'icalendar)

(defcustom icalendar-uid-format
  "emacs%t%c"
  "Format of unique ID code (UID) for each iCalendar object.
The following specifiers are available:
%c COUNTER, an integer value that is increased each time a uid is
   generated.  This may be necessary for systems which do not
   provide time-resolution finer than a second.
%h HASH, a hash value of the diary entry,
%s DTSTART, the start date (excluding time) of the diary entry,
%t TIMESTAMP, a unique creation timestamp,
%u USERNAME, the variable `user-login-name'.

For example, a value of \"%s_%h@mydomain.com\" will generate a
UID code for each entry composed of the time of the event, a hash
code for the event, and your personal domain name."
  :type 'string
  :group 'icalendar)

(defvar icalendar-debug nil
  "Enable icalendar debug messages.")

;; ======================================================================
;; NO USER SERVICEABLE PARTS BELOW THIS LINE
;; ======================================================================

(defconst icalendar--weekday-array ["SU" "MO" "TU" "WE" "TH" "FR" "SA"])

;; ======================================================================
;; all the other libs we need
;; ======================================================================
(require 'calendar)
(require 'diary-lib)

;; ======================================================================
;; misc
;; ======================================================================
(defun icalendar--dmsg (&rest args)
  "Print message ARGS if `icalendar-debug' is non-nil."
  (if icalendar-debug
      (apply 'message args)))

;; ======================================================================
;; Core functionality
;; Functions for parsing icalendars, importing and so on
;; ======================================================================

(defun icalendar--get-unfolded-buffer (folded-ical-buffer)
  "Return a new buffer containing the unfolded contents of a buffer.
Folding is the iCalendar way of wrapping long lines.  In the
created buffer all occurrences of CR LF BLANK are replaced by the
empty string.  Argument FOLDED-ICAL-BUFFER is the unfolded input
buffer."
  (let ((unfolded-buffer (get-buffer-create " *icalendar-work*")))
    (save-current-buffer
      (set-buffer unfolded-buffer)
      (erase-buffer)
      (insert-buffer-substring folded-ical-buffer)
      (goto-char (point-min))
      (while (re-search-forward "\r?\n[ \t]" nil t)
        (replace-match "" nil nil)))
    unfolded-buffer))

(defsubst icalendar--rris (regexp rep string &optional fixedcase literal)
  "Replace regular expression in string.
Pass arguments REGEXP REP STRING FIXEDCASE LITERAL to
`replace-regexp-in-string' (Emacs) or to `replace-in-string' (XEmacs)."
  (cond ((fboundp 'replace-regexp-in-string)
         ;; Emacs:
         (replace-regexp-in-string regexp rep string fixedcase literal))
        ((fboundp 'replace-in-string)
         ;; XEmacs:
         (save-match-data ;; apparently XEmacs needs save-match-data
           (replace-in-string string regexp rep literal)))))

(defun icalendar--read-element (invalue inparams)
  "Recursively read the next iCalendar element in the current buffer.
INVALUE gives the current iCalendar element we are reading.
INPARAMS gives the current parameters.....
This function calls itself recursively for each nested calendar element
it finds."
  (let (element children line name params param param-name param-value
                value
                (continue t))
    (setq children '())
    (while (and continue
                (re-search-forward "^\\([A-Za-z0-9-]+\\)[;:]" nil t))
      (setq name (intern (match-string 1)))
      (backward-char 1)
      (setq params '())
      (setq line '())
      (while (looking-at ";")
        (re-search-forward ";\\([A-Za-z0-9-]+\\)=" nil nil)
        (setq param-name (intern (match-string 1)))
        (re-search-forward "\\(\\([^;,:\"]+\\)\\|\"\\([^\"]+\\)\"\\)[;:]"
                           nil t)
        (backward-char 1)
        (setq param-value (or (match-string 2) (match-string 3)))
        (setq param (list param-name param-value))
        (while (looking-at ",")
          (re-search-forward "\\(\\([^;,:]+\\)\\|\"\\([^\"]+\\)\"\\)"
                             nil t)
          (if (match-string 2)
              (setq param-value (match-string 2))
            (setq param-value (match-string 3)))
          (setq param (append param param-value)))
        (setq params (append params param)))
      (unless (looking-at ":")
        (error "Oops"))
      (forward-char 1)
      (re-search-forward  "\\(.*\\)\\(\r?\n[ \t].*\\)*" nil t)
      (setq value (icalendar--rris "\r?\n[ \t]" "" (match-string 0)))
      (setq line (list name params value))
      (cond ((eq name 'BEGIN)
             (setq children
                   (append children
                           (list (icalendar--read-element (intern value)
                                                          params)))))
            ((eq name 'END)
             (setq continue nil))
            (t
             (setq element (append element (list line))))))
    (if invalue
        (list invalue inparams element children)
      children)))

;; ======================================================================
;; helper functions for examining events
;; ======================================================================

;;(defsubst icalendar--get-all-event-properties (event)
;;  "Return the list of properties in this EVENT."
;;  (car (cddr event)))

(defun icalendar--get-event-property (event prop)
  "For the given EVENT return the value of the first occurrence of PROP."
  (catch 'found
    (let ((props (car (cddr event))) pp)
      (while props
        (setq pp (car props))
        (if (eq (car pp) prop)
            (throw 'found (car (cddr pp))))
        (setq props (cdr props))))
    nil))

(defun icalendar--get-event-property-attributes (event prop)
  "For the given EVENT return attributes of the first occurrence of PROP."
  (catch 'found
    (let ((props (car (cddr event))) pp)
      (while props
        (setq pp (car props))
        (if (eq (car pp) prop)
            (throw 'found (cadr pp)))
        (setq props (cdr props))))
    nil))

(defun icalendar--get-event-properties (event prop)
  "For the given EVENT return a list of all values of the property PROP."
  (let ((props (car (cddr event))) pp result)
    (while props
      (setq pp (car props))
      (if (eq (car pp) prop)
          (setq result (append (split-string (car (cddr pp)) ",") result)))
      (setq props (cdr props)))
    result))

;; (defun icalendar--set-event-property (event prop new-value)
;;   "For the given EVENT set the property PROP to the value NEW-VALUE."
;;   (catch 'found
;;     (let ((props (car (cddr event))) pp)
;;       (while props
;;         (setq pp (car props))
;;         (when (eq (car pp) prop)
;;           (setcdr (cdr pp) new-value)
;;           (throw 'found (car (cddr pp))))
;;         (setq props (cdr props)))
;;       (setq props (car (cddr event)))
;;       (setcar (cddr event)
;;               (append props (list (list prop nil new-value)))))))

(defun icalendar--get-children (node name)
  "Return all children of the given NODE which have a name NAME.
For instance the VCALENDAR node can have VEVENT children as well as VTODO
children."
  (let ((result nil)
        (children (cadr (cddr node))))
    (when (eq (car node) name)
      (setq result node))
    ;;(message "%s" node)
    (when children
      (let ((subresult
             (delq nil
                   (mapcar (lambda (n)
                             (icalendar--get-children n name))
                           children))))
        (if subresult
            (if result
                (setq result (append result subresult))
              (setq result subresult)))))
    result))

;; private
(defun icalendar--all-events (icalendar)
  "Return the list of all existing events in the given ICALENDAR."
  (let ((result '()))
    (mapc (lambda (elt)
	    (setq result (append (icalendar--get-children elt 'VEVENT)
                                 result)))
	  (nreverse icalendar))
    result))

(defun icalendar--split-value (value-string)
  "Split VALUE-STRING at ';='."
  (let ((result '())
        param-name param-value)
    (when value-string
      (save-current-buffer
        (set-buffer (get-buffer-create " *icalendar-work*"))
        (set-buffer-modified-p nil)
        (erase-buffer)
        (insert value-string)
        (goto-char (point-min))
        (while
            (re-search-forward
             "\\([A-Za-z0-9-]+\\)=\\(\\([^;:]+\\)\\|\"\\([^\"]+\\)\"\\);?"
             nil t)
          (setq param-name (intern (match-string 1)))
          (setq param-value (match-string 2))
          (setq result
                (append result (list (list param-name param-value)))))))
    result))

(defun icalendar--convert-tz-offset (alist dst-p)
  "Return a cons of two strings representing a timezone start.
ALIST is an alist entry from a VTIMEZONE, like STANDARD.
DST-P is non-nil if this is for daylight savings time.
The strings are suitable for assembling into a TZ variable."
  (let ((offset (car (cddr (assq 'TZOFFSETTO alist))))
	(rrule-value (car (cddr (assq 'RRULE alist))))
	(dtstart (car (cddr (assq 'DTSTART alist)))))
    ;; FIXME: for now we only handle RRULE and not RDATE here.
    (when (and offset rrule-value dtstart)
      (let* ((rrule (icalendar--split-value rrule-value))
	     (freq (cadr (assq 'FREQ rrule)))
	     (bymonth (cadr (assq 'BYMONTH rrule)))
	     (byday (cadr (assq 'BYDAY rrule))))
	;; FIXME: we don't correctly handle WKST here.
	(if (and (string= freq "YEARLY") bymonth)
	    (cons
	     (concat
	      ;; Fake a name.
	      (if dst-p "DST" "STD")
	      ;; For TZ, OFFSET is added to the local time.  So,
	      ;; invert the values.
	      (if (eq (aref offset 0) ?-) "+" "-")
	      (substring offset 1 3)
	      ":"
	      (substring offset 3 5))
	     ;; The start time.
	     (let* ((day (icalendar--get-weekday-number (substring byday -2)))
		    (week (if (eq day -1)
			      byday
			    (substring byday 0 -2))))
               ;; "Translate" the iCalendar way to specify the last
               ;; (sun|mon|...)day in month to the tzset way.
               (if (string= week "-1")  ; last day as iCalendar calls it
                   (setq week "5"))     ; last day as tzset calls it
	       (concat "M" bymonth "." week "." (if (eq day -1) "0"
						  (int-to-string day))
		       ;; Start time.
		       "/"
		       (substring dtstart -6 -4)
		       ":"
		       (substring dtstart -4 -2)
		       ":"
		       (substring dtstart -2)))))))))

(defun icalendar--parse-vtimezone (alist)
  "Turn a VTIMEZONE ALIST into a cons (ID . TZ-STRING).
Return nil if timezone cannot be parsed."
  (let* ((tz-id (icalendar--get-event-property alist 'TZID))
	 (daylight (cadr (cdar (icalendar--get-children alist 'DAYLIGHT))))
	 (day (and daylight (icalendar--convert-tz-offset daylight t)))
	 (standard (cadr (cdar (icalendar--get-children alist 'STANDARD))))
	 (std (and standard (icalendar--convert-tz-offset standard nil))))
    (if (and tz-id std)
	(cons tz-id
	      (if day
		  (concat (car std) (car day)
			  "," (cdr day) "," (cdr std))
		(car std))))))

(defun icalendar--convert-all-timezones (icalendar)
  "Convert all timezones in the ICALENDAR into an alist.
Each element of the alist is a cons (ID . TZ-STRING),
like `icalendar--parse-vtimezone'."
  (let (result)
    (dolist (zone (icalendar--get-children (car icalendar) 'VTIMEZONE))
      (setq zone (icalendar--parse-vtimezone zone))
      (if zone
	  (setq result (cons zone result))))
    result))

(defun icalendar--find-time-zone (prop-list zone-map)
  "Return a timezone string for the time zone in PROP-LIST, or nil if none.
ZONE-MAP is a timezone alist as returned by `icalendar--convert-all-timezones'."
  (let ((id (plist-get prop-list 'TZID)))
    (if id
	(cdr (assoc id zone-map)))))

(defun icalendar--decode-isodatetime (isodatetimestring &optional day-shift
                                                        zone)
  "Return ISODATETIMESTRING in format like `decode-time'.
Converts from ISO-8601 to Emacs representation.  If
ISODATETIMESTRING specifies UTC time (trailing letter Z) the
decoded time is given in the local time zone!  If optional
parameter DAY-SHIFT is non-nil the result is shifted by DAY-SHIFT
days.
ZONE, if provided, is the timezone, in any format understood by `encode-time'.

FIXME: multiple comma-separated values should be allowed!"
  (icalendar--dmsg isodatetimestring)
  (if isodatetimestring
      ;; day/month/year must be present
      (let ((year  (read (substring isodatetimestring 0 4)))
            (month (read (substring isodatetimestring 4 6)))
            (day   (read (substring isodatetimestring 6 8)))
            (hour 0)
            (minute 0)
            (second 0))
        (when (> (length isodatetimestring) 12)
          ;; hour/minute present
          (setq hour (read (substring isodatetimestring 9 11)))
          (setq minute (read (substring isodatetimestring 11 13))))
        (when (> (length isodatetimestring) 14)
          ;; seconds present
          (setq second (read (substring isodatetimestring 13 15))))
        (when (and (> (length isodatetimestring) 15)
                   ;; UTC specifier present
                   (char-equal ?Z (aref isodatetimestring 15)))
          ;; if not UTC add current-time-zone offset
          (setq second (+ (car (current-time-zone)) second)))
        ;; shift if necessary
        (if day-shift
            (let ((mdy (calendar-gregorian-from-absolute
                        (+ (calendar-absolute-from-gregorian
                            (list month day year))
                           day-shift))))
              (setq month (nth 0 mdy))
              (setq day   (nth 1 mdy))
              (setq year  (nth 2 mdy))))
        ;; create the decoded date-time
        ;; FIXME!?!
        (condition-case nil
            (decode-time (encode-time second minute hour day month year zone))
          (error
           (message "Cannot decode \"%s\"" isodatetimestring)
           ;; hope for the best...
           (list second minute hour day month year 0 nil 0))))
    ;; isodatetimestring == nil
    nil))

(defun icalendar--decode-isoduration (isodurationstring
                                      &optional duration-correction)
  "Convert ISODURATIONSTRING into format provided by `decode-time'.
Converts from ISO-8601 to Emacs representation.  If ISODURATIONSTRING
specifies UTC time (trailing letter Z) the decoded time is given in
the local time zone!

Optional argument DURATION-CORRECTION shortens result by one day.

FIXME: TZID-attributes are ignored....!
FIXME: multiple comma-separated values should be allowed!"
  (if isodurationstring
      (save-match-data
        (string-match
         (concat
          "^P[+-]?\\("
          "\\(\\([0-9]+\\)D\\)"         ; days only
          "\\|"
          "\\(\\(\\([0-9]+\\)D\\)?T\\(\\([0-9]+\\)H\\)?" ; opt days
          "\\(\\([0-9]+\\)M\\)?\\(\\([0-9]+\\)S\\)?\\)"  ; mand. time
          "\\|"
          "\\(\\([0-9]+\\)W\\)"         ; weeks only
          "\\)$") isodurationstring)
        (let ((seconds 0)
              (minutes 0)
              (hours 0)
              (days 0)
              (months 0)
              (years 0))
          (cond
           ((match-beginning 2)         ;days only
            (setq days (read (substring isodurationstring
                                        (match-beginning 3)
                                        (match-end 3))))
            (when duration-correction
              (setq days (1- days))))
           ((match-beginning 4)         ;days and time
            (if (match-beginning 5)
                (setq days (* 7 (read (substring isodurationstring
                                                 (match-beginning 6)
                                                 (match-end 6))))))
            (if (match-beginning 7)
                (setq hours (read (substring isodurationstring
                                             (match-beginning 8)
                                             (match-end 8)))))
            (if (match-beginning 9)
                (setq minutes (read (substring isodurationstring
                                               (match-beginning 10)
                                               (match-end 10)))))
            (if (match-beginning 11)
                (setq seconds (read (substring isodurationstring
                                               (match-beginning 12)
                                               (match-end 12))))))
           ((match-beginning 13)        ;weeks only
            (setq days (* 7 (read (substring isodurationstring
                                             (match-beginning 14)
                                             (match-end 14)))))))
          (list seconds minutes hours days months years)))
    ;; isodatetimestring == nil
    nil))

(defun icalendar--add-decoded-times (time1 time2)
  "Add TIME1 to TIME2.
Both times must be given in decoded form.  One of these times must be
valid (year > 1900 or something)."
  ;; FIXME: does this function exist already?
  (decode-time (encode-time
                (+ (nth 0 time1) (nth 0 time2))
                (+ (nth 1 time1) (nth 1 time2))
                (+ (nth 2 time1) (nth 2 time2))
                (+ (nth 3 time1) (nth 3 time2))
                (+ (nth 4 time1) (nth 4 time2))
                (+ (nth 5 time1) (nth 5 time2))
                nil
                nil
                ;;(or (nth 6 time1) (nth 6 time2)) ;; FIXME?
                )))

(defun icalendar--datetime-to-american-date (datetime &optional separator)
  "Convert the decoded DATETIME to American-style format.
Optional argument SEPARATOR gives the separator between month,
day, and year.  If nil a blank character is used as separator.
American format: \"month day year\"."
  (if datetime
      (format "%d%s%d%s%d" (nth 4 datetime) ;month
              (or separator " ")
              (nth 3 datetime)          ;day
              (or separator " ")
              (nth 5 datetime))         ;year
    ;; datetime == nil
    nil))

(define-obsolete-function-alias 'icalendar--datetime-to-noneuropean-date
  'icalendar--datetime-to-american-date "icalendar 0.19")

(defun icalendar--datetime-to-european-date (datetime &optional separator)
  "Convert the decoded DATETIME to European format.
Optional argument SEPARATOR gives the separator between month,
day, and year.  If nil a blank character is used as separator.
European format: (day month year).
FIXME"
  (if datetime
      (format "%d%s%d%s%d" (nth 3 datetime) ;day
              (or separator " ")
              (nth 4 datetime)            ;month
              (or separator " ")
              (nth 5 datetime))           ;year
    ;; datetime == nil
    nil))

(defun icalendar--datetime-to-iso-date (datetime &optional separator)
  "Convert the decoded DATETIME to ISO format.
Optional argument SEPARATOR gives the separator between month,
day, and year.  If nil a blank character is used as separator.
ISO format: (year month day)."
  (if datetime
      (format "%d%s%d%s%d" (nth 5 datetime) ;year
              (or separator " ")
              (nth 4 datetime)            ;month
              (or separator " ")
              (nth 3 datetime))           ;day
    ;; datetime == nil
    nil))

(defun icalendar--date-style ()
  "Return current calendar date style.
Convenience function to handle transition from old
`european-calendar-style' to new `calendar-date-style'."
  (if (boundp 'calendar-date-style)
      calendar-date-style
    (if (with-no-warnings european-calendar-style)
        'european
      'american)))

(defun icalendar--datetime-to-diary-date (datetime &optional separator)
  "Convert the decoded DATETIME to diary format.
Optional argument SEPARATOR gives the separator between month,
day, and year.  If nil a blank character is used as separator.
Call icalendar--datetime-to-*-date according to the current
calendar date style."
  (funcall (intern-soft (format "icalendar--datetime-to-%s-date"
                                (icalendar--date-style)))
           datetime separator))

(defun icalendar--datetime-to-colontime (datetime)
  "Extract the time part of a decoded DATETIME into 24-hour format.
Note that this silently ignores seconds."
  (format "%02d:%02d" (nth 2 datetime) (nth 1 datetime)))

(defun icalendar--get-month-number (monthname)
  "Return the month number for the given MONTHNAME."
  (catch 'found
    (let ((num 1)
          (m (downcase monthname)))
      (mapc (lambda (month)
              (let ((mm (downcase month)))
                (if (or (string-equal mm m)
                        (string-equal (substring mm 0 3) m))
                    (throw 'found num))
                (setq num (1+ num))))
            calendar-month-name-array))
    ;; Error:
    -1))

(defun icalendar--get-weekday-number (abbrevweekday)
  "Return the number for the ABBREVWEEKDAY."
  (if abbrevweekday
      (catch 'found
        (let ((num 0)
              (aw (downcase abbrevweekday)))
          (mapc (lambda (day)
                  (let ((d (downcase day)))
                    (if (string-equal d aw)
                        (throw 'found num))
                    (setq num (1+ num))))
                icalendar--weekday-array)))
    ;; Error:
    -1))

(defun icalendar--get-weekday-numbers (abbrevweekdays)
  "Return the list of numbers for the comma-separated ABBREVWEEKDAYS."
  (when abbrevweekdays
    (let* ((num -1)
           (weekday-alist (mapcar (lambda (day)
                                    (progn
                                      (setq num (1+ num))
                                      (cons (downcase day) num)))
                                  icalendar--weekday-array)))
      (delq nil
            (mapcar (lambda (abbrevday)
                      (cdr (assoc abbrevday weekday-alist)))
                    (split-string (downcase abbrevweekdays) ","))))))

(defun icalendar--get-weekday-abbrev (weekday)
  "Return the abbreviated WEEKDAY."
  (catch 'found
    (let ((num 0)
          (w (downcase weekday)))
      (mapc (lambda (day)
              (let ((d (downcase day)))
                (if (or (string-equal d w)
                        (string-equal (substring d 0 3) w))
                    (throw 'found (aref icalendar--weekday-array num)))
                (setq num (1+ num))))
            calendar-day-name-array))
    ;; Error:
    nil))

(defun icalendar--date-to-isodate (date &optional day-shift)
  "Convert DATE to iso-style date.
DATE must be a list of the form (month day year).
If DAY-SHIFT is non-nil, the result is shifted by DAY-SHIFT days."
  (let ((mdy (calendar-gregorian-from-absolute
              (+ (calendar-absolute-from-gregorian date)
                 (or day-shift 0)))))
    (format "%04d%02d%02d" (nth 2 mdy) (nth 0 mdy) (nth 1 mdy))))


(defun icalendar--datestring-to-isodate (datestring &optional day-shift)
  "Convert diary-style DATESTRING to iso-style date.
If DAY-SHIFT is non-nil, the result is shifted by DAY-SHIFT days
-- DAY-SHIFT must be either nil or an integer.  This function
tries to figure the date style from DATESTRING itself.  If that
is not possible it uses the current calendar date style."
  (let ((day -1) month year)
    (save-match-data
      (cond ( ;; iso-style numeric date
             (string-match (concat "\\s-*"
                                   "\\([0-9]\\{4\\}\\)[ \t/]\\s-*"
                                   "0?\\([1-9][0-9]?\\)[ \t/]\\s-*"
                                   "0?\\([1-9][0-9]?\\)")
                           datestring)
             (setq year (read (substring datestring (match-beginning 1)
                                         (match-end 1))))
             (setq month (read (substring datestring (match-beginning 2)
                                          (match-end 2))))
             (setq day (read (substring datestring (match-beginning 3)
                                        (match-end 3)))))
            ( ;; non-iso numeric date -- must rely on configured
              ;; calendar style
             (string-match (concat "\\s-*"
                                   "0?\\([1-9][0-9]?\\)[ \t/]\\s-*"
                                   "0?\\([1-9][0-9]?\\),?[ \t/]\\s-*"
                                   "\\([0-9]\\{4\\}\\)")
                           datestring)
             (setq day (read (substring datestring (match-beginning 1)
                                        (match-end 1))))
             (setq month (read (substring datestring (match-beginning 2)
                                          (match-end 2))))
             (setq year (read (substring datestring (match-beginning 3)
                                         (match-end 3))))
             (if (eq (icalendar--date-style) 'american)
                 (let ((x month))
                   (setq month day)
                   (setq day x))))
            ( ;; date contains month names -- iso style
             (string-match (concat "\\s-*"
                                   "\\([0-9]\\{4\\}\\)[ \t/]\\s-*"
                                   "\\([A-Za-z][^ ]+\\)[ \t/]\\s-*"
                                   "0?\\([123]?[0-9]\\)")
                           datestring)
             (setq year (read (substring datestring (match-beginning 1)
                                        (match-end 1))))
             (setq month (icalendar--get-month-number
                          (substring datestring (match-beginning 2)
                                     (match-end 2))))
             (setq day (read (substring datestring (match-beginning 3)
                                        (match-end 3)))))
            ( ;; date contains month names -- european style
             (string-match (concat "\\s-*"
                                   "0?\\([123]?[0-9]\\)[ \t/]\\s-*"
                                   "\\([A-Za-z][^ ]+\\)[ \t/]\\s-*"
                                   "\\([0-9]\\{4\\}\\)")
                           datestring)
             (setq day (read (substring datestring (match-beginning 1)
                                        (match-end 1))))
             (setq month (icalendar--get-month-number
                          (substring datestring (match-beginning 2)
                                     (match-end 2))))
             (setq year (read (substring datestring (match-beginning 3)
                                         (match-end 3)))))
            ( ;; date contains month names -- american style
             (string-match (concat "\\s-*"
                                   "\\([A-Za-z][^ ]+\\)[ \t/]\\s-*"
                                   "0?\\([123]?[0-9]\\),?[ \t/]\\s-*"
                                   "\\([0-9]\\{4\\}\\)")
                           datestring)
             (setq day (read (substring datestring (match-beginning 2)
                                        (match-end 2))))
             (setq month (icalendar--get-month-number
                          (substring datestring (match-beginning 1)
                                     (match-end 1))))
             (setq year (read (substring datestring (match-beginning 3)
                                         (match-end 3)))))
            (t
             nil)))
    (if (> day 0)
        (let ((mdy (calendar-gregorian-from-absolute
                    (+ (calendar-absolute-from-gregorian (list month day
                                                               year))
                       (or day-shift 0)))))
          (icalendar--dmsg (format "%04d%02d%02d" (nth 2 mdy) (nth 0 mdy) (nth 1 mdy)))
          (format "%04d%02d%02d" (nth 2 mdy) (nth 0 mdy) (nth 1 mdy)))
      nil)))

(defun icalendar--diarytime-to-isotime (timestring ampmstring)
  "Convert a time like 9:30pm to an iso-conform string like T213000.
In this example the TIMESTRING would be \"9:30\" and the AMPMSTRING
would be \"pm\"."
  (if timestring
      (let ((starttimenum (read (icalendar--rris ":" "" timestring))))
        ;; take care of am/pm style
        ;; Be sure *not* to convert 12:00pm - 12:59pm to 2400-2459
        (if (and ampmstring (string= "pm" ampmstring) (< starttimenum 1200))
            (setq starttimenum (+ starttimenum 1200)))
	;; Similar effect with 12:00am - 12:59am (need to convert to 0000-0059)
        (if (and ampmstring (string= "am" ampmstring) (>= starttimenum 1200))
            (setq starttimenum (- starttimenum 1200)))
        (format "T%04d00" starttimenum))
    nil))

(defun icalendar--convert-string-for-export (string)
  "Escape comma and other critical characters in STRING."
  (icalendar--rris "," "\\\\," string))

(defun icalendar--convert-string-for-import (string)
  "Remove escape chars for comma, semicolon etc. from STRING."
  (icalendar--rris
   "\\\\n" "\n " (icalendar--rris
                  "\\\\\"" "\"" (icalendar--rris
                                 "\\\\;" ";" (icalendar--rris
                                              "\\\\," "," string)))))

;; ======================================================================
;; Export -- convert emacs-diary to iCalendar
;; ======================================================================

;;;###autoload
(defun icalendar-export-file (diary-filename ical-filename)
  "Export diary file to iCalendar format.
All diary entries in the file DIARY-FILENAME are converted to iCalendar
format.  The result is appended to the file ICAL-FILENAME."
  (interactive "FExport diary data from file: \n\
Finto iCalendar file: ")
  (save-current-buffer
    (set-buffer (find-file diary-filename))
    (icalendar-export-region (point-min) (point-max) ical-filename)))

(defalias 'icalendar-convert-diary-to-ical 'icalendar-export-file)
(make-obsolete 'icalendar-convert-diary-to-ical 'icalendar-export-file "22.1")

(defvar icalendar--uid-count 0
  "Auxiliary counter for creating unique ids.")

(defun icalendar--create-uid (entry-full contents)
  "Construct a unique iCalendar UID for a diary entry.
ENTRY-FULL is the full diary entry string.  CONTENTS is the
current iCalendar object, as a string.  Increase
`icalendar--uid-count'.  Returns the UID string."
  (let ((uid icalendar-uid-format))
    (if
	;; Allow other apps (such as org-mode) to create its own uid
	(get-text-property 0 'uid entry-full)
	(setq uid (get-text-property 0 'uid entry-full))
      (setq uid (replace-regexp-in-string
                 "%c"
                 (format "%d" icalendar--uid-count)
                 uid t t))
      (setq icalendar--uid-count (1+ icalendar--uid-count))
      (setq uid (replace-regexp-in-string
                 "%t"
                 (format "%d%d%d" (car (current-time))
                         (cadr (current-time))
                         (car (cddr (current-time))))
                 uid t t))
      (setq uid (replace-regexp-in-string
                 "%h"
                 (format "%d" (abs (sxhash entry-full))) uid t t))
      (setq uid (replace-regexp-in-string
                 "%u" (or user-login-name "UNKNOWN_USER") uid t t))
      (let ((dtstart (if (string-match "^DTSTART[^:]*:\\([0-9]*\\)" contents)
                         (substring contents (match-beginning 1) (match-end 1))
                       "DTSTART")))
        (setq uid (replace-regexp-in-string "%s" dtstart uid t t))))

    ;; Return the UID string
    uid))

;;;###autoload
(defun icalendar-export-region (min max ical-filename)
  "Export region in diary file to iCalendar format.
All diary entries in the region from MIN to MAX in the current buffer are
converted to iCalendar format.  The result is appended to the file
ICAL-FILENAME.
This function attempts to return t if something goes wrong.  In this
case an error string which describes all the errors and problems is
written into the buffer `*icalendar-errors*'."
  (interactive "r
FExport diary data into iCalendar file: ")
  (let ((result "")
        (start 0)
        (entry-main "")
        (entry-rest "")
	(entry-full "")
        (header "")
        (contents-n-summary)
        (contents)
        (found-error nil)
        (nonmarker (concat "^" (regexp-quote diary-nonmarking-symbol)
                           "?"))
        (other-elements nil))
    ;; prepare buffer with error messages
    (save-current-buffer
      (set-buffer (get-buffer-create "*icalendar-errors*"))
      (erase-buffer))

    ;; here we go
    (save-excursion
      (goto-char min)
      (while (re-search-forward
              ;; possibly ignore hidden entries beginning with "&"
              (if icalendar-export-hidden-diary-entries
                  "^\\([^ \t\n#].+\\)\\(\\(\n[ \t].*\\)*\\)"
                "^\\([^ \t\n&#].+\\)\\(\\(\n[ \t].*\\)*\\)") max t)
        (setq entry-main (match-string 1))
        (if (match-beginning 2)
            (setq entry-rest (match-string 2))
          (setq entry-rest ""))
	(setq entry-full (concat entry-main entry-rest))

        (condition-case error-val
            (progn
              (setq contents-n-summary
                    (icalendar--convert-to-ical nonmarker entry-main))
              (setq other-elements (icalendar--parse-summary-and-rest
				    entry-full))
              (setq contents (concat (car contents-n-summary)
                                     "\nSUMMARY:" (cadr contents-n-summary)))
              (let ((cla (cdr (assoc 'cla other-elements)))
                    (des (cdr (assoc 'des other-elements)))
                    (loc (cdr (assoc 'loc other-elements)))
                    (org (cdr (assoc 'org other-elements)))
                    (sta (cdr (assoc 'sta other-elements)))
                    (sum (cdr (assoc 'sum other-elements)))
                    (url (cdr (assoc 'url other-elements))))
                (if cla
                    (setq contents (concat contents "\nCLASS:" cla)))
                (if des
                    (setq contents (concat contents "\nDESCRIPTION:" des)))
                (if loc
                    (setq contents (concat contents "\nLOCATION:" loc)))
                (if org
                    (setq contents (concat contents "\nORGANIZER:" org)))
                (if sta
                    (setq contents (concat contents "\nSTATUS:" sta)))
                ;;(if sum
                ;;    (setq contents (concat contents "\nSUMMARY:" sum)))
                (if url
                    (setq contents (concat contents "\nURL:" url))))

	      (setq header (concat "\nBEGIN:VEVENT\nUID:"
				   (icalendar--create-uid entry-full contents)))
              (setq result (concat result header contents "\nEND:VEVENT")))
          ;; handle errors
          (error
           (setq found-error t)
           (save-current-buffer
             (set-buffer (get-buffer-create "*icalendar-errors*"))
             (insert (format "Error in line %d -- %s: `%s'\n"
                             (count-lines (point-min) (point))
                             error-val
                             entry-main))))))

      ;; we're done, insert everything into the file
      (save-current-buffer
        (let ((coding-system-for-write 'utf-8))
          (set-buffer (find-file ical-filename))
          (goto-char (point-max))
          (insert "BEGIN:VCALENDAR")
          (insert "\nPRODID:-//Emacs//NONSGML icalendar.el//EN")
          (insert "\nVERSION:2.0")
          (insert result)
          (insert "\nEND:VCALENDAR\n")
          ;; save the diary file
          (save-buffer)
          (unless found-error
            (bury-buffer)))))
    found-error))

(defun icalendar--convert-to-ical (nonmarker entry-main)
  "Convert a diary entry to iCalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (or
   ;; anniversaries -- %%(diary-anniversary ...)
   (icalendar--convert-anniversary-to-ical nonmarker entry-main)
   ;; cyclic events -- %%(diary-cyclic ...)
   (icalendar--convert-cyclic-to-ical nonmarker entry-main)
   ;; diary-date -- %%(diary-date ...)
   (icalendar--convert-date-to-ical nonmarker entry-main)
   ;; float events -- %%(diary-float ...)
   (icalendar--convert-float-to-ical nonmarker entry-main)
   ;; block events -- %%(diary-block ...)
   (icalendar--convert-block-to-ical nonmarker entry-main)
   ;; other sexp diary entries
   (icalendar--convert-sexp-to-ical nonmarker entry-main)
   ;; weekly by day -- Monday 8:30 Team meeting
   (icalendar--convert-weekly-to-ical nonmarker entry-main)
   ;; yearly by day -- 1 May Tag der Arbeit
   (icalendar--convert-yearly-to-ical nonmarker entry-main)
   ;; "ordinary" events, start and end time given
   ;; 1 Feb 2003 blah
   (icalendar--convert-ordinary-to-ical nonmarker entry-main)
   ;; everything else
   ;; Oops! what's that?
   (error "Could not parse entry")))

(defun icalendar--parse-summary-and-rest (summary-and-rest)
  "Parse SUMMARY-AND-REST from a diary to fill iCalendar properties.
Returns an alist."
  (save-match-data
    (if (functionp icalendar-import-format)
        ;; can't do anything
        nil
      ;; split summary-and-rest
      (let* ((s icalendar-import-format)
             (p-cla (or (string-match "%c" icalendar-import-format) -1))
             (p-des (or (string-match "%d" icalendar-import-format) -1))
             (p-loc (or (string-match "%l" icalendar-import-format) -1))
             (p-org (or (string-match "%o" icalendar-import-format) -1))
             (p-sum (or (string-match "%s" icalendar-import-format) -1))
             (p-sta (or (string-match "%t" icalendar-import-format) -1))
             (p-url (or (string-match "%u" icalendar-import-format) -1))
             (p-list (sort (list p-cla p-des p-loc p-org p-sta p-sum p-url) '<))
	     (ct 0)
             pos-cla pos-des pos-loc pos-org pos-sta pos-sum pos-url)
        (dotimes (i (length p-list))
	  ;; Use 'ct' to keep track of current position in list
          (cond ((and (>= p-cla 0) (= (nth i p-list) p-cla))
		 (setq ct (+ ct 1))
                 (setq pos-cla (* 2 ct)))
                ((and (>= p-des 0) (= (nth i p-list) p-des))
		 (setq ct (+ ct 1))
                 (setq pos-des (* 2 ct)))
                ((and (>= p-loc 0) (= (nth i p-list) p-loc))
		 (setq ct (+ ct 1))
                 (setq pos-loc (* 2 ct)))
                ((and (>= p-org 0) (= (nth i p-list) p-org))
		 (setq ct (+ ct 1))
                 (setq pos-org (* 2 ct)))
                ((and (>= p-sta 0) (= (nth i p-list) p-sta))
		 (setq ct (+ ct 1))
                 (setq pos-sta (* 2 ct)))
                ((and (>= p-sum 0) (= (nth i p-list) p-sum))
		 (setq ct (+ ct 1))
                 (setq pos-sum (* 2 ct)))
                ((and (>= p-url 0) (= (nth i p-list) p-url))
		 (setq ct (+ ct 1))
                 (setq pos-url (* 2 ct)))) )
        (mapc (lambda (ij)
                (setq s (icalendar--rris (car ij) (cadr ij) s t t)))
              (list
               ;; summary must be first! because of %s
               (list "%s"
                     (concat "\\(" icalendar-import-format-summary "\\)??"))
               (list "%c"
                     (concat "\\(" icalendar-import-format-class "\\)??"))
               (list "%d"
                     (concat "\\(" icalendar-import-format-description "\\)??"))
               (list "%l"
                     (concat "\\(" icalendar-import-format-location "\\)??"))
               (list "%o"
                     (concat "\\(" icalendar-import-format-organizer "\\)??"))
               (list "%t"
                     (concat "\\(" icalendar-import-format-status "\\)??"))
               (list "%u"
                     (concat "\\(" icalendar-import-format-url "\\)??"))))
	;; Need the \' regexp in order to detect multi-line items
        (setq s (concat "\\`"
			   (icalendar--rris "%s" "\\(.*?\\)" s nil t)
                        "\\'"))
        (if (string-match s summary-and-rest)
            (let (cla des loc org sta sum url)
              (if (and pos-sum (match-beginning pos-sum))
                  (setq sum (substring summary-and-rest
                                       (match-beginning pos-sum)
                                       (match-end pos-sum))))
              (if (and pos-cla (match-beginning pos-cla))
                  (setq cla (substring summary-and-rest
                                       (match-beginning pos-cla)
                                       (match-end pos-cla))))
              (if (and pos-des (match-beginning pos-des))
                  (setq des (substring summary-and-rest
                                       (match-beginning pos-des)
                                       (match-end pos-des))))
              (if (and pos-loc (match-beginning pos-loc))
                  (setq loc (substring summary-and-rest
                                       (match-beginning pos-loc)
                                       (match-end pos-loc))))
              (if (and pos-org (match-beginning pos-org))
                  (setq org (substring summary-and-rest
                                       (match-beginning pos-org)
                                       (match-end pos-org))))
              (if (and pos-sta (match-beginning pos-sta))
                  (setq sta (substring summary-and-rest
                                       (match-beginning pos-sta)
                                       (match-end pos-sta))))
              (if (and pos-url (match-beginning pos-url))
                  (setq url (substring summary-and-rest
                                       (match-beginning pos-url)
                                       (match-end pos-url))))
              (list (if cla (cons 'cla cla) nil)
                    (if des (cons 'des des) nil)
                    (if loc (cons 'loc loc) nil)
                    (if org (cons 'org org) nil)
                    (if sta (cons 'sta sta) nil)
                    ;;(if sum (cons 'sum sum) nil)
                    (if url (cons 'url url) nil))))))))

;; subroutines for icalendar-export-region
(defun icalendar--convert-ordinary-to-ical (nonmarker entry-main)
  "Convert \"ordinary\" diary entry to iCalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match
       (concat nonmarker
               "\\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\)\\s-*" ; date
               "\\(\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?" ; start time
               "\\("
               "-\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?" ; end time
               "\\)?"
               "\\s-*\\(.*?\\) ?$")
       entry-main)
      (let* ((datetime (substring entry-main (match-beginning 1)
                                  (match-end 1)))
             (startisostring (icalendar--datestring-to-isodate
                              datetime))
             (endisostring (icalendar--datestring-to-isodate
                            datetime 1))
             (endisostring1)
             (starttimestring (icalendar--diarytime-to-isotime
                               (if (match-beginning 3)
                                   (substring entry-main
                                              (match-beginning 3)
                                              (match-end 3))
                                 nil)
                               (if (match-beginning 4)
                                   (substring entry-main
                                              (match-beginning 4)
                                              (match-end 4))
                                 nil)))
             (endtimestring (icalendar--diarytime-to-isotime
                             (if (match-beginning 6)
                                 (substring entry-main
                                            (match-beginning 6)
                                            (match-end 6))
                               nil)
                             (if (match-beginning 7)
                                 (substring entry-main
                                            (match-beginning 7)
                                            (match-end 7))
                               nil)))
             (summary (icalendar--convert-string-for-export
                       (substring entry-main (match-beginning 8)
                                  (match-end 8)))))
        (icalendar--dmsg "ordinary %s" entry-main)

        (unless startisostring
          (error "Could not parse date"))

        ;; If only start-date is specified, then end-date is next day,
        ;; otherwise it is same day.
        (setq endisostring1 (if starttimestring
                                startisostring
                              endisostring))

        (when starttimestring
          (unless endtimestring
            (let ((time
                   (read (icalendar--rris "^T0?" ""
                                          starttimestring))))
              (if (< time 230000)
                  ;; Case: ends on same day
              (setq endtimestring (format "T%06d"
                                              (+ 10000 time)))
                ;; Case: ends on next day
                (setq endtimestring (format "T%06d"
                                              (- time 230000)))
                (setq endisostring1 endisostring)) )))

        (list (concat "\nDTSTART;"
                      (if starttimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      startisostring
                      (or starttimestring "")
                      "\nDTEND;"
                      (if endtimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      endisostring1
                      (or endtimestring ""))
              summary))
    ;; no match
    nil))

(defun icalendar-first-weekday-of-year (abbrevweekday year)
  "Find the first ABBREVWEEKDAY in a given YEAR.
Returns day number."
  (let* ((day-of-week-jan01 (calendar-day-of-week (list 1 1 year)))
         (result (+ 1
                    (- (icalendar--get-weekday-number abbrevweekday)
                       day-of-week-jan01))))
    (cond ((<= result 0)
           (setq result (+ result 7)))
          ((> result 7)
           (setq result (- result 7))))
    result))

(defun icalendar--convert-weekly-to-ical (nonmarker entry-main)
  "Convert weekly diary entry to iCalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (and (string-match (concat nonmarker
                                 "\\([a-z]+\\)\\s-+"
                                 "\\(\\([0-9][0-9]?:[0-9][0-9]\\)"
                                 "\\([ap]m\\)?"
                                 "\\(-"
                                 "\\([0-9][0-9]?:[0-9][0-9]\\)"
                                 "\\([ap]m\\)?\\)?"
                                 "\\)?"
                                 "\\s-*\\(.*?\\) ?$")
                         entry-main)
           (icalendar--get-weekday-abbrev
            (substring entry-main (match-beginning 1)
                       (match-end 1))))
      (let* ((day (icalendar--get-weekday-abbrev
                   (substring entry-main (match-beginning 1)
                              (match-end 1))))
             (starttimestring (icalendar--diarytime-to-isotime
                               (if (match-beginning 3)
                                   (substring entry-main
                                              (match-beginning 3)
                                              (match-end 3))
                                 nil)
                               (if (match-beginning 4)
                                   (substring entry-main
                                              (match-beginning 4)
                                              (match-end 4))
                                 nil)))
             (endtimestring (icalendar--diarytime-to-isotime
                             (if (match-beginning 6)
                                 (substring entry-main
                                            (match-beginning 6)
                                            (match-end 6))
                               nil)
                             (if (match-beginning 7)
                                 (substring entry-main
                                            (match-beginning 7)
                                            (match-end 7))
                               nil)))
             (summary (icalendar--convert-string-for-export
                       (substring entry-main (match-beginning 8)
                                  (match-end 8)))))
        (icalendar--dmsg "weekly %s" entry-main)

        (when starttimestring
          (unless endtimestring
            (let ((time (read
                         (icalendar--rris "^T0?" ""
                                          starttimestring))))
              (setq endtimestring (format "T%06d"
                                          (+ 10000 time))))))
        (list (concat "\nDTSTART;"
                      (if starttimestring
                          "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      ;; Find the first requested weekday of the
                      ;; start year
                      (funcall 'format "%04d%02d%02d"
                               icalendar-recurring-start-year 1
                               (icalendar-first-weekday-of-year
                                day icalendar-recurring-start-year))
                      (or starttimestring "")
                      "\nDTEND;"
                      (if endtimestring
                          "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      (funcall 'format "%04d%02d%02d"
                               ;; end is non-inclusive!
                               icalendar-recurring-start-year 1
                               (+ (icalendar-first-weekday-of-year
                                   day icalendar-recurring-start-year)
                          (if endtimestring 0 1)))
                      (or endtimestring "")
                      "\nRRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY="
                      day)
              summary))
    ;; no match
    nil))

(defun icalendar--convert-yearly-to-ical (nonmarker entry-main)
  "Convert yearly diary entry to iCalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match (concat nonmarker
                            (if (eq (icalendar--date-style) 'european)
                                "\\([0-9]+[0-9]?\\)\\s-+\\([a-z]+\\)\\s-+"
                              "\\([a-z]+\\)\\s-+\\([0-9]+[0-9]?\\)\\s-+")
                            "\\*?\\s-*"
                            "\\(\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
                            "\\("
                            "-\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
                            "\\)?"
                            "\\s-*\\([^0-9]+.*?\\) ?$" ; must not match years
                            )
                    entry-main)
      (let* ((daypos (if (eq (icalendar--date-style) 'european) 1 2))
             (monpos (if (eq (icalendar--date-style) 'european) 2 1))
             (day (read (substring entry-main
                                   (match-beginning daypos)
                                   (match-end daypos))))
             (month (icalendar--get-month-number
                     (substring entry-main
                                (match-beginning monpos)
                                (match-end monpos))))
             (starttimestring (icalendar--diarytime-to-isotime
                               (if (match-beginning 4)
                                   (substring entry-main
                                              (match-beginning 4)
                                              (match-end 4))
                                 nil)
                               (if (match-beginning 5)
                                   (substring entry-main
                                              (match-beginning 5)
                                              (match-end 5))
                                 nil)))
             (endtimestring (icalendar--diarytime-to-isotime
                             (if (match-beginning 7)
                                 (substring entry-main
                                            (match-beginning 7)
                                            (match-end 7))
                               nil)
                             (if (match-beginning 8)
                                 (substring entry-main
                                            (match-beginning 8)
                                            (match-end 8))
                               nil)))
             (summary (icalendar--convert-string-for-export
                       (substring entry-main (match-beginning 9)
                                  (match-end 9)))))
        (icalendar--dmsg "yearly %s" entry-main)

        (when starttimestring
          (unless endtimestring
            (let ((time (read
                         (icalendar--rris "^T0?" ""
                                          starttimestring))))
              (setq endtimestring (format "T%06d"
                                          (+ 10000 time))))))
        (list (concat "\nDTSTART;"
                      (if starttimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      (format "1900%02d%02d" month day)
                      (or starttimestring "")
                      "\nDTEND;"
                      (if endtimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      ;; end is not included! shift by one day
                      (icalendar--date-to-isodate
                       (list month day 1900)
                       (if endtimestring 0 1))
                      (or endtimestring "")
                      "\nRRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH="
                      (format "%d" month)
                      ";BYMONTHDAY="
                      (format "%d" day))
              summary))
    ;; no match
    nil))

(defun icalendar--convert-sexp-to-ical (nonmarker entry-main)
  "Convert complex sexp diary entry to iCalendar format -- unsupported!

FIXME!

NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (cond ((string-match (concat nonmarker
                               "%%(and \\(([^)]+)\\))\\(\\s-*.*?\\) ?$")
                       entry-main)
         ;; simple sexp entry as generated by icalendar.el: strip off the
         ;; unnecessary (and)
         (icalendar--dmsg "diary-sexp from icalendar.el %s" entry-main)
         (icalendar--convert-to-ical
          nonmarker
          (concat "%%"
                  (substring entry-main (match-beginning 1) (match-end 1))
                  (substring entry-main (match-beginning 2) (match-end 2)))))
        ((string-match (concat nonmarker
                               "%%([^)]+)\\s-*.*")
                       entry-main)
         (icalendar--dmsg "diary-sexp %s" entry-main)
         (error "Sexp-entries are not supported yet"))
        (t
         ;; no match
         nil)))

(defun icalendar--convert-block-to-ical (nonmarker entry-main)
  "Convert block diary entry to iCalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match (concat nonmarker
                            "%%(diary-block \\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\)"
                            " +\\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\))\\s-*"
                            "\\(\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
                            "\\("
                            "-\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
                            "\\)?"
                            "\\s-*\\(.*?\\) ?$")
                    entry-main)
      (let* ((startstring (substring entry-main
                                     (match-beginning 1)
                                     (match-end 1)))
             (endstring (substring entry-main
                                   (match-beginning 2)
                                   (match-end 2)))
             (startisostring (icalendar--datestring-to-isodate
                              startstring))
             (endisostring (icalendar--datestring-to-isodate
                            endstring))
             (endisostring+1 (icalendar--datestring-to-isodate
                              endstring 1))
             (starttimestring (icalendar--diarytime-to-isotime
                               (if (match-beginning 4)
                                   (substring entry-main
                                              (match-beginning 4)
                                              (match-end 4))
                                 nil)
                               (if (match-beginning 5)
                                   (substring entry-main
                                              (match-beginning 5)
                                              (match-end 5))
                                 nil)))
             (endtimestring (icalendar--diarytime-to-isotime
                             (if (match-beginning 7)
                                 (substring entry-main
                                            (match-beginning 7)
                                            (match-end 7))
                               nil)
                             (if (match-beginning 8)
                                 (substring entry-main
                                            (match-beginning 8)
                                            (match-end 8))
                               nil)))
             (summary (icalendar--convert-string-for-export
                       (substring entry-main (match-beginning 9)
                                  (match-end 9)))))
        (icalendar--dmsg "diary-block %s" entry-main)
        (when starttimestring
          (unless endtimestring
            (let ((time
                   (read (icalendar--rris "^T0?" ""
                                          starttimestring))))
              (setq endtimestring (format "T%06d"
                                          (+ 10000 time))))))
        (if starttimestring
            ;; with time -> write rrule
            (list (concat "\nDTSTART;VALUE=DATE-TIME:"
                          startisostring
                          starttimestring
                          "\nDTEND;VALUE=DATE-TIME:"
                          startisostring
                          endtimestring
                          "\nRRULE:FREQ=DAILY;INTERVAL=1;UNTIL="
                          endisostring)
                  summary)
          ;; no time -> write long event
          (list (concat "\nDTSTART;VALUE=DATE:" startisostring
                        "\nDTEND;VALUE=DATE:" endisostring+1)
                summary)))
    ;; no match
    nil))

(defun icalendar--convert-float-to-ical (nonmarker entry-main)
  "Convert float diary entry to iCalendar format -- partially unsupported!

  FIXME! DAY from diary-float yet unimplemented.

  NONMARKER is a regular expression matching the start of non-marking
  entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match (concat nonmarker "%%\\((diary-float .+\\) ?$") entry-main)
      (with-temp-buffer
        (insert (match-string 1 entry-main))
        (goto-char (point-min))
        (let* ((sexp (read (current-buffer))) ;using `read' here
					      ;easier than regexp
					      ;matching, esp. with
					      ;different forms of
					      ;MONTH
               (month (nth 1 sexp))
               (dayname (nth 2 sexp))
               (n (nth 3 sexp))
               (day (nth 4 sexp))
               (summary
		(replace-regexp-in-string
		 "\\(^\s+\\|\s+$\\)" ""
		 (buffer-substring (point) (point-max)))))

          (when day
            (progn
              (icalendar--dmsg "diary-float %s" entry-main)
              (error "Don't know if or how to implement day in `diary-float'")))

          (list (concat
                 ;;Start today (yes this is an arbitrary choice):
                 "\nDTSTART;VALUE=DATE:"
                 (format-time-string "%Y%m%d" (current-time))
                 ;;BUT remove today if `diary-float'
                 ;;expression does not hold true for today:
                 (when
                     (null (let ((date (calendar-current-date))
                                 (entry entry-main))
                             (diary-float month dayname n)))
                   (concat
                    "\nEXDATE;VALUE=DATE:"
                    (format-time-string "%Y%m%d" (current-time))))
                 "\nRRULE:"
                 (if (or (numberp month) (listp month))
                     "FREQ=YEARLY;BYMONTH="
                   "FREQ=MONTHLY")
                 (when
                     (listp month)
                   (mapconcat
                    (lambda (m)
                      (number-to-string m))
                    (cadr month) ","))
                 (when
                     (numberp month)
                   (number-to-string month))
                 ";BYDAY="
                 (number-to-string n)
		 (aref icalendar--weekday-array dayname))
                summary)))
    ;; no match
    nil))

(defun icalendar--convert-date-to-ical (nonmarker entry-main)
  "Convert `diary-date' diary entry to iCalendar format -- unsupported!

FIXME!

NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match (concat nonmarker
                            "%%(diary-date \\([^)]+\\))\\s-*\\(.*?\\) ?$")
                    entry-main)
      (progn
        (icalendar--dmsg "diary-date %s" entry-main)
        (error "`diary-date' is not supported yet"))
    ;; no match
    nil))

(defun icalendar--convert-cyclic-to-ical (nonmarker entry-main)
  "Convert `diary-cyclic' diary entry to iCalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match (concat nonmarker
                            "%%(diary-cyclic \\([^ ]+\\) +"
                            "\\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\))\\s-*"
                            "\\(\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
                            "\\("
                            "-\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
                            "\\)?"
                            "\\s-*\\(.*?\\) ?$")
                    entry-main)
      (let* ((frequency (substring entry-main (match-beginning 1)
                                   (match-end 1)))
             (datetime (substring entry-main (match-beginning 2)
                                  (match-end 2)))
             (startisostring (icalendar--datestring-to-isodate
                              datetime))
             (endisostring (icalendar--datestring-to-isodate
                            datetime))
             (endisostring+1 (icalendar--datestring-to-isodate
                              datetime 1))
             (starttimestring (icalendar--diarytime-to-isotime
                               (if (match-beginning 4)
                                   (substring entry-main
                                              (match-beginning 4)
                                              (match-end 4))
                                 nil)
                               (if (match-beginning 5)
                                   (substring entry-main
                                              (match-beginning 5)
                                              (match-end 5))
                                 nil)))
             (endtimestring (icalendar--diarytime-to-isotime
                             (if (match-beginning 7)
                                 (substring entry-main
                                            (match-beginning 7)
                                            (match-end 7))
                               nil)
                             (if (match-beginning 8)
                                 (substring entry-main
                                            (match-beginning 8)
                                            (match-end 8))
                               nil)))
             (summary (icalendar--convert-string-for-export
                       (substring entry-main (match-beginning 9)
                                  (match-end 9)))))
        (icalendar--dmsg "diary-cyclic %s" entry-main)
        (when starttimestring
          (unless endtimestring
            (let ((time
                   (read (icalendar--rris "^T0?" ""
                                          starttimestring))))
              (setq endtimestring (format "T%06d"
                                          (+ 10000 time))))))
        (list (concat "\nDTSTART;"
                      (if starttimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      startisostring
                      (or starttimestring "")
                      "\nDTEND;"
                      (if endtimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      (if endtimestring endisostring endisostring+1)
                      (or endtimestring "")
                      "\nRRULE:FREQ=DAILY;INTERVAL=" frequency
                      ;; strange: korganizer does not expect
                      ;; BYSOMETHING here...
                      )
              summary))
    ;; no match
    nil))

(defun icalendar--convert-anniversary-to-ical (nonmarker entry-main)
  "Convert `diary-anniversary' diary entry to iCalendar format.
NONMARKER is a regular expression matching the start of non-marking
entries.  ENTRY-MAIN is the first line of the diary entry."
  (if (string-match (concat nonmarker
                            "%%(diary-anniversary \\([^)]+\\))\\s-*"
                            "\\(\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
                            "\\("
                            "-\\([0-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
                            "\\)?"
                            "\\s-*\\(.*?\\) ?$")
                    entry-main)
      (let* ((datetime (substring entry-main (match-beginning 1)
                                  (match-end 1)))
             (startisostring (icalendar--datestring-to-isodate
                              datetime))
             (endisostring (icalendar--datestring-to-isodate
                            datetime 1))
             (starttimestring (icalendar--diarytime-to-isotime
                               (if (match-beginning 3)
                                   (substring entry-main
                                              (match-beginning 3)
                                              (match-end 3))
                                 nil)
                               (if (match-beginning 4)
                                   (substring entry-main
                                              (match-beginning 4)
                                              (match-end 4))
                                 nil)))
             (endtimestring (icalendar--diarytime-to-isotime
                             (if (match-beginning 6)
                                 (substring entry-main
                                            (match-beginning 6)
                                            (match-end 6))
                               nil)
                             (if (match-beginning 7)
                                 (substring entry-main
                                            (match-beginning 7)
                                            (match-end 7))
                               nil)))
             (summary (icalendar--convert-string-for-export
                       (substring entry-main (match-beginning 8)
                                  (match-end 8)))))
        (icalendar--dmsg "diary-anniversary %s" entry-main)
        (when starttimestring
          (unless endtimestring
            (let ((time
                   (read (icalendar--rris "^T0?" ""
                                          starttimestring))))
              (setq endtimestring (format "T%06d"
                                          (+ 10000 time))))))
        (list (concat "\nDTSTART;"
                      (if starttimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      startisostring
                      (or starttimestring "")
                      "\nDTEND;"
                      (if endtimestring "VALUE=DATE-TIME:"
                        "VALUE=DATE:")
                      endisostring
                      (or endtimestring "")
                      "\nRRULE:FREQ=YEARLY;INTERVAL=1"
                      ;; the following is redundant,
                      ;; but korganizer seems to expect this... ;(
                      ;; and evolution doesn't understand it... :(
                      ;; so... who is wrong?!
                      ";BYMONTH="
                      (substring startisostring 4 6)
                      ";BYMONTHDAY="
                      (substring startisostring 6 8))
              summary))
    ;; no match
    nil))

;; ======================================================================
;; Import -- convert iCalendar to emacs-diary
;; ======================================================================

;;;###autoload
(defun icalendar-import-file (ical-filename diary-filename
                                            &optional non-marking)
  "Import an iCalendar file and append to a diary file.
Argument ICAL-FILENAME output iCalendar file.
Argument DIARY-FILENAME input `diary-file'.
Optional argument NON-MARKING determines whether events are created as
non-marking or not."
  (interactive "fImport iCalendar data from file: \n\
Finto diary file:
p")
  ;; clean up the diary file
  (save-current-buffer
    ;; now load and convert from the ical file
    (set-buffer (find-file ical-filename))
    (icalendar-import-buffer diary-filename t non-marking)))

;;;###autoload
(defun icalendar-import-buffer (&optional diary-file do-not-ask
                                          non-marking)
  "Extract iCalendar events from current buffer.

This function searches the current buffer for the first iCalendar
object, reads it and adds all VEVENT elements to the diary
DIARY-FILE.

It will ask for each appointment whether to add it to the diary
unless DO-NOT-ASK is non-nil.  When called interactively,
DO-NOT-ASK is nil, so that you are asked for each event.

NON-MARKING determines whether diary events are created as
non-marking.

Return code t means that importing worked well, return code nil
means that an error has occurred.  Error messages will be in the
buffer `*icalendar-errors*'."
  (interactive)
  (save-current-buffer
    ;; prepare ical
    (message "Preparing iCalendar...")
    (set-buffer (icalendar--get-unfolded-buffer (current-buffer)))
    (goto-char (point-min))
    (message "Preparing iCalendar...done")
    (if (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (let (ical-contents ical-errors)
          ;; read ical
          (message "Reading iCalendar...")
          (beginning-of-line)
          (setq ical-contents (icalendar--read-element nil nil))
          (message "Reading iCalendar...done")
          ;; convert ical
          (message "Converting iCalendar...")
          (setq ical-errors (icalendar--convert-ical-to-diary
                             ical-contents
                             diary-file do-not-ask non-marking))
          (when diary-file
            ;; save the diary file if it is visited already
            (let ((b (find-buffer-visiting diary-file)))
              (when b
                (save-current-buffer
                  (set-buffer b)
                  (save-buffer)))))
          (message "Converting iCalendar...done")
          ;; return t if no error occurred
          (not ical-errors))
      (message
       "Current buffer does not contain iCalendar contents!")
      ;; return nil, i.e. import did not work
      nil)))

(defalias 'icalendar-extract-ical-from-buffer 'icalendar-import-buffer)
(make-obsolete 'icalendar-extract-ical-from-buffer 'icalendar-import-buffer "22.1")

(defun icalendar--format-ical-event (event)
  "Create a string representation of an iCalendar EVENT."
  (if (functionp icalendar-import-format)
      (funcall icalendar-import-format event)
    (let ((string icalendar-import-format)
        (conversion-list
         '(("%c" CLASS       icalendar-import-format-class)
           ("%d" DESCRIPTION icalendar-import-format-description)
           ("%l" LOCATION    icalendar-import-format-location)
           ("%o" ORGANIZER   icalendar-import-format-organizer)
           ("%s" SUMMARY     icalendar-import-format-summary)
           ("%t" STATUS      icalendar-import-format-status)
           ("%u" URL         icalendar-import-format-url))))
    ;; convert the specifiers in the format string
    (mapc (lambda (i)
	    (let* ((spec (car i))
		   (prop (cadr i))
		   (format (car (cddr i)))
		   (contents (icalendar--get-event-property event prop))
		   (formatted-contents ""))
	      (when (and contents (> (length contents) 0))
		(setq formatted-contents
		      (icalendar--rris "%s"
				       (icalendar--convert-string-for-import
					contents)
				       (symbol-value format)
				       t t)))
	      (setq string (icalendar--rris spec
					    formatted-contents
					    string
					    t t))))
	  conversion-list)
    string)))

(defun icalendar--convert-ical-to-diary (ical-list diary-file
                                                   &optional do-not-ask
                                                   non-marking)
  "Convert iCalendar data to an Emacs diary file.
Import VEVENTS from the iCalendar object ICAL-LIST and saves them to a
DIARY-FILE.  If DO-NOT-ASK is nil the user is asked for each event
whether to actually import it.  NON-MARKING determines whether diary
events are created as non-marking.
This function attempts to return t if something goes wrong.  In this
case an error string which describes all the errors and problems is
written into the buffer `*icalendar-errors*'."
  (let* ((ev (icalendar--all-events ical-list))
         (error-string "")
         (event-ok t)
         (found-error nil)
         (zone-map (icalendar--convert-all-timezones ical-list))
         e diary-string)
    ;; step through all events/appointments
    (while ev
      (setq e (car ev))
      (setq ev (cdr ev))
      (setq event-ok nil)
      (condition-case error-val
          (let* ((dtstart (icalendar--get-event-property e 'DTSTART))
                 (dtstart-zone (icalendar--find-time-zone
 				(icalendar--get-event-property-attributes
 				 e 'DTSTART)
 				zone-map))
                 (dtstart-dec (icalendar--decode-isodatetime dtstart nil
                                                             dtstart-zone))
                 (start-d (icalendar--datetime-to-diary-date
                           dtstart-dec))
                 (start-t (icalendar--datetime-to-colontime dtstart-dec))
                 (dtend (icalendar--get-event-property e 'DTEND))
                 (dtend-zone (icalendar--find-time-zone
 			      (icalendar--get-event-property-attributes
 			       e 'DTEND)
 			      zone-map))
                 (dtend-dec (icalendar--decode-isodatetime dtend
                                                           nil dtend-zone))
                 (dtend-1-dec (icalendar--decode-isodatetime dtend -1
                                                             dtend-zone))
                 end-d
                 end-1-d
                 end-t
                 (summary (icalendar--convert-string-for-import
                           (or (icalendar--get-event-property e 'SUMMARY)
                               "No summary")))
                 (rrule (icalendar--get-event-property e 'RRULE))
                 (rdate (icalendar--get-event-property e 'RDATE))
                 (duration (icalendar--get-event-property e 'DURATION)))
            (icalendar--dmsg "%s: `%s'" start-d summary)
            ;; check whether start-time is missing
            (if  (and dtstart
                      (string=
                       (cadr (icalendar--get-event-property-attributes
                              e 'DTSTART))
                       "DATE"))
                (setq start-t nil))
            (when duration
              (let ((dtend-dec-d (icalendar--add-decoded-times
                                  dtstart-dec
                                  (icalendar--decode-isoduration duration)))
                    (dtend-1-dec-d (icalendar--add-decoded-times
                                    dtstart-dec
                                    (icalendar--decode-isoduration duration
                                                                   t))))
                (if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
                    (message "Inconsistent endtime and duration for %s"
                             summary))
                (setq dtend-dec dtend-dec-d)
                (setq dtend-1-dec dtend-1-dec-d)))
            (setq end-d (if dtend-dec
                            (icalendar--datetime-to-diary-date dtend-dec)
                          start-d))
            (setq end-1-d (if dtend-1-dec
                              (icalendar--datetime-to-diary-date dtend-1-dec)
                            start-d))
            (setq end-t (if (and
                             dtend-dec
                             (not (string=
                                   (cadr
                                    (icalendar--get-event-property-attributes
                                     e 'DTEND))
                                   "DATE")))
                            (icalendar--datetime-to-colontime dtend-dec)
                          start-t))
            (icalendar--dmsg "start-d: %s, end-d: %s" start-d end-d)
            (cond
             ;; recurring event
             (rrule
              (setq diary-string
                    (icalendar--convert-recurring-to-diary e dtstart-dec start-t
                                                           end-t))
              (setq event-ok t))
             (rdate
              (icalendar--dmsg "rdate event")
              (setq diary-string "")
              (mapc (lambda (datestring)
		      (setq diary-string
			    (concat diary-string
				    (format "......"))))
		    (icalendar--split-value rdate)))
             ;; non-recurring event
             ;; all-day event
             ((not (string= start-d end-d))
              (setq diary-string
                    (icalendar--convert-non-recurring-all-day-to-diary
                     e start-d end-1-d))
              (setq event-ok t))
             ;; not all-day
             ((and start-t (or (not end-t)
                               (not (string= start-t end-t))))
              (setq diary-string
                    (icalendar--convert-non-recurring-not-all-day-to-diary
                     e dtstart-dec dtend-dec start-t end-t))
              (setq event-ok t))
             ;; all-day event
             (t
              (icalendar--dmsg "all day event")
              (setq diary-string (icalendar--datetime-to-diary-date
                                  dtstart-dec "/"))
              (setq event-ok t)))
            ;; add all other elements unless the user doesn't want to have
            ;; them
            (if event-ok
                (progn
                  (setq diary-string
                        (concat diary-string " "
                                (icalendar--format-ical-event e)))
                  (if do-not-ask (setq summary nil))
                  ;; add entry to diary and store actual name of diary
                  ;; file (in case it was nil)
                  (setq diary-file
                        (icalendar--add-diary-entry diary-string diary-file
                                                    non-marking summary)))
              ;; event was not ok
              (setq found-error t)
              (setq error-string
                    (format "%s\nCannot handle this event:%s"
                            error-string e))))
        ;; FIXME: inform user about ignored event properties
        ;; handle errors
        (error
         (message "Ignoring event \"%s\"" e)
         (setq found-error t)
         (setq error-string (format "%s\n%s\nCannot handle this event: %s"
                                    error-val error-string e))
         (message "%s" error-string))))

    ;; insert final newline
    (if diary-file
        (let ((b (find-buffer-visiting diary-file)))
          (when b
            (save-current-buffer
              (set-buffer b)
              (goto-char (point-max))
              (insert "\n")))))
    (if found-error
        (save-current-buffer
          (set-buffer (get-buffer-create "*icalendar-errors*"))
          (erase-buffer)
          (insert error-string)))
    (message "Converting iCalendar...done")
    found-error))

;; subroutines for importing
(defun icalendar--convert-recurring-to-diary (e dtstart-dec start-t end-t)
  "Convert recurring iCalendar event E to diary format.

DTSTART-DEC is the DTSTART property of E.
START-T is the event's start time in diary format.
END-T is the event's end time in diary format."
  (icalendar--dmsg "recurring event")
  (let* ((rrule        (icalendar--get-event-property e 'RRULE))
         (rrule-props  (icalendar--split-value rrule))
         (frequency    (cadr (assoc 'FREQ rrule-props)))
         (until        (cadr (assoc 'UNTIL rrule-props)))
         (count        (cadr (assoc 'COUNT rrule-props)))
         (interval     (read (or (cadr (assoc 'INTERVAL rrule-props)) "1")))
         (dtstart-conv (icalendar--datetime-to-diary-date dtstart-dec))
         (until-conv   (icalendar--datetime-to-diary-date
                        (icalendar--decode-isodatetime until)))
         (until-1-conv (icalendar--datetime-to-diary-date
                        (icalendar--decode-isodatetime until -1)))
         (result  ""))

    ;; FIXME FIXME interval!!!!!!!!!!!!!

    (when count
      (if until
          (message "Must not have UNTIL and COUNT -- ignoring COUNT element!")
        (let ((until-1 0))
          (cond ((string-equal frequency "DAILY")
                 (setq until (icalendar--add-decoded-times
                              dtstart-dec
                              (list 0 0 0 (* (read count) interval) 0 0)))
                 (setq until-1 (icalendar--add-decoded-times
                                dtstart-dec
                                (list 0 0 0 (* (- (read count) 1) interval)
                                      0 0)))
                 )
                ((string-equal frequency "WEEKLY")
                 (setq until (icalendar--add-decoded-times
                              dtstart-dec
                              (list 0 0 0 (* (read count) 7 interval) 0 0)))
                 (setq until-1 (icalendar--add-decoded-times
                                dtstart-dec
                                (list 0 0 0 (* (- (read count) 1) 7
                                               interval) 0 0)))
                 )
                ((string-equal frequency "MONTHLY")
                 (setq until (icalendar--add-decoded-times
                              dtstart-dec (list 0 0 0 0 (* (- (read count) 1)
                                                           interval) 0)))
                 (setq until-1 (icalendar--add-decoded-times
                                dtstart-dec (list 0 0 0 0 (* (- (read count) 1)
                                                             interval) 0)))
                 )
                ((string-equal frequency "YEARLY")
                 (setq until (icalendar--add-decoded-times
                              dtstart-dec (list 0 0 0 0 0 (* (- (read count) 1)
                                                             interval))))
                 (setq until-1 (icalendar--add-decoded-times
                                dtstart-dec
                                (list 0 0 0 0 0 (* (- (read count) 1)
                                                   interval))))
                 )
                (t
                 (message "Cannot handle COUNT attribute for `%s' events."
                          frequency)))
          (setq until-conv (icalendar--datetime-to-diary-date until))
          (setq until-1-conv (icalendar--datetime-to-diary-date until-1))
          ))
      )
    (cond ((string-equal frequency "WEEKLY")
	   (let* ((byday (cadr (assoc 'BYDAY rrule-props)))
		  (weekdays
		   (icalendar--get-weekday-numbers byday))
		  (weekday-clause
		   (when (> (length weekdays) 1)
		     (format "(memq (calendar-day-of-week date) '%s) "
			     weekdays))))
	     (if (not start-t)
		 (progn
		   ;; weekly and all-day
		   (icalendar--dmsg "weekly all-day")
		   (if until
		       (setq result
			     (format
			      (concat "%%%%(and "
				      "%s"
				      "(diary-block %s %s))")
			      (or weekday-clause
				  (format "(diary-cyclic %d %s) "
					  (* interval 7)
					  dtstart-conv))
			      dtstart-conv
			      (if count until-1-conv until-conv)
			      ))
		       (setq result
			     (format "%%%%(and %s(diary-cyclic %d %s))"
				     (or weekday-clause "")
				     (if weekday-clause 1 (* interval 7))
				     dtstart-conv))))
               ;; weekly and not all-day
               (icalendar--dmsg "weekly not-all-day")
               (if until
                   (setq result
                         (format
                          (concat "%%%%(and "
                                  "%s"
                                  "(diary-block %s %s)) "
                                  "%s%s%s")
			  (or weekday-clause
			      (format "(diary-cyclic %d %s) "
				      (* interval 7)
				      dtstart-conv))
                          dtstart-conv
                          until-conv
                          (or start-t "")
                          (if end-t "-" "") (or end-t "")))
                 ;; no limit
                 ;; FIXME!!!!
                 ;; DTSTART;VALUE=DATE-TIME:20030919T090000
                 ;; DTEND;VALUE=DATE-TIME:20030919T113000
                 (setq result
                       (format
                        "%%%%(and %s(diary-cyclic %d %s)) %s%s%s"
			(or weekday-clause "")
			(if weekday-clause 1 (* interval 7))
			dtstart-conv
			(or start-t "")
                        (if end-t "-" "") (or end-t "")))))))
          ;; yearly
          ((string-equal frequency "YEARLY")
           (icalendar--dmsg "yearly")
           (if until
               (let ((day (nth 3 dtstart-dec))
                     (month (nth 4 dtstart-dec)))
                 (setq result (concat "%%(and (diary-date "
                                      (cond ((eq (icalendar--date-style) 'iso)
                                             (format "t %d %d" month day))
                                            ((eq (icalendar--date-style) 'european)
                                             (format "%d %d t" day month))
                                            ((eq (icalendar--date-style) 'american)
                                             (format "%d %d t" month day)))
                                      ") (diary-block "
                                      dtstart-conv
                                      " "
                                      until-conv
                                      ")) "
                                      (or start-t "")
                                      (if end-t "-" "")
                                      (or end-t ""))))
             (setq result (format
                           "%%%%(and (diary-anniversary %s)) %s%s%s"
                           dtstart-conv
                           (or start-t "")
                           (if end-t "-" "") (or end-t "")))))
          ;; monthly
          ((string-equal frequency "MONTHLY")
           (icalendar--dmsg "monthly")
           (setq result
                 (format
                  "%%%%(and (diary-date %s) (diary-block %s %s)) %s%s%s"
                  (let ((day (nth 3 dtstart-dec)))
                    (cond ((eq (icalendar--date-style) 'iso)
                           (format "t t %d" day))
                          ((eq (icalendar--date-style) 'european)
                           (format "%d t t" day))
                          ((eq (icalendar--date-style) 'american)
                           (format "t %d t" day))))
                  dtstart-conv
                  (if until
                      until-conv
                    (if (eq (icalendar--date-style) 'iso) "9999 1 1" "1 1 9999")) ;; FIXME: should be unlimited
                  (or start-t "")
                  (if end-t "-" "") (or end-t ""))))
          ;; daily
          ((and (string-equal frequency "DAILY"))
           (if until
               (setq result
                     (format
                      (concat "%%%%(and (diary-cyclic %s %s) "
                              "(diary-block %s %s)) %s%s%s")
                      interval dtstart-conv dtstart-conv
                      (if count until-1-conv until-conv)
                      (or start-t "")
                      (if end-t "-" "") (or end-t "")))
             (setq result
                   (format
                    "%%%%(and (diary-cyclic %s %s)) %s%s%s"
                    interval
                    dtstart-conv
                    (or start-t "")
                    (if end-t "-" "") (or end-t ""))))))
    ;; Handle exceptions from recurrence rules
    (let ((ex-dates (icalendar--get-event-properties e 'EXDATE)))
      (while ex-dates
        (let* ((ex-start (icalendar--decode-isodatetime
                          (car ex-dates)))
               (ex-d (icalendar--datetime-to-diary-date
                      ex-start)))
          (setq result
                (icalendar--rris "^%%(\\(and \\)?"
                                 (format
                                  "%%%%(and (not (diary-date %s)) "
                                  ex-d)
                                 result)))
        (setq ex-dates (cdr ex-dates))))
    ;; FIXME: exception rules are not recognized
    (if (icalendar--get-event-property e 'EXRULE)
        (setq result
              (concat result
                      "\n Exception rules: "
                      (icalendar--get-event-properties
                       e 'EXRULE))))
    result))

(defun icalendar--convert-non-recurring-all-day-to-diary (event start-d end-d)
  "Convert non-recurring iCalendar EVENT to diary format.

DTSTART is the decoded DTSTART property of E.
Argument START-D gives the first day.
Argument END-D gives the last day."
  (icalendar--dmsg "non-recurring all-day event")
  (format "%%%%(and (diary-block %s %s))" start-d end-d))

(defun icalendar--convert-non-recurring-not-all-day-to-diary (event dtstart-dec
                                                                    dtend-dec
                                                                    start-t
                                                                    end-t)
  "Convert recurring icalendar EVENT to diary format.

DTSTART-DEC is the decoded DTSTART property of E.
DTEND-DEC is the decoded DTEND property of E.
START-T is the event's start time in diary format.
END-T is the event's end time in diary format."
  (icalendar--dmsg "not all day event")
  (cond (end-t
         (format "%s %s-%s"
                 (icalendar--datetime-to-diary-date
                  dtstart-dec "/")
                 start-t end-t))
        (t
         (format "%s %s"
                 (icalendar--datetime-to-diary-date
                  dtstart-dec "/")
                 start-t))))

(defun icalendar--add-diary-entry (string diary-file non-marking
                                          &optional summary)
  "Add STRING to the diary file DIARY-FILE.
STRING must be a properly formatted valid diary entry.  NON-MARKING
determines whether diary events are created as non-marking.  If
SUMMARY is not nil it must be a string that gives the summary of the
entry.  In this case the user will be asked whether he wants to insert
the entry."
  (when (or (not summary)
            (y-or-n-p (format "Add appointment for `%s' to diary? "
                              summary)))
    (when summary
      (setq non-marking
            (y-or-n-p (format "Make appointment non-marking? "))))
    (save-window-excursion
      (unless diary-file
        (setq diary-file
              (read-file-name "Add appointment to this diary file: ")))
      ;; Note: diary-make-entry will add a trailing blank char.... :(
      (funcall (if (fboundp 'diary-make-entry)
                   'diary-make-entry
                 'make-diary-entry)
               string non-marking diary-file)))
  ;; Würgaround to remove the trailing blank char
  (with-current-buffer (find-file diary-file)
    (goto-char (point-max))
    (if (= (char-before) ? )
        (delete-char -1)))
  ;; return diary-file in case it has been changed interactively
  diary-file)

;; ======================================================================
;; Examples
;; ======================================================================
(defun icalendar-import-format-sample (event)
  "Example function for formatting an iCalendar EVENT."
  (format (concat "SUMMARY=`%s' DESCRIPTION=`%s' LOCATION=`%s' ORGANIZER=`%s' "
                  "STATUS=`%s' URL=`%s' CLASS=`%s'")
          (or (icalendar--get-event-property event 'SUMMARY) "")
          (or (icalendar--get-event-property event 'DESCRIPTION) "")
          (or (icalendar--get-event-property event 'LOCATION) "")
          (or (icalendar--get-event-property event 'ORGANIZER) "")
          (or (icalendar--get-event-property event 'STATUS) "")
          (or (icalendar--get-event-property event 'URL) "")
          (or (icalendar--get-event-property event 'CLASS) "")))

(provide 'icalendar)

;;; icalendar.el ends here
