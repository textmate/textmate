;;; timezone.el --- time zone package for GNU Emacs

;; Copyright (C) 1990-1993, 1996, 1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Masanobu Umeda
;; Maintainer: umerin@mse.kyutech.ac.jp
;; Keywords: news

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

;;; Code:

(defvar timezone-world-timezones
  '(("PST" .  -800)
    ("PDT" .  -700)
    ("MST" .  -700)
    ("MDT" .  -600)
    ("CST" .  -600)
    ("CDT" .  -500)
    ("EST" .  -500)
    ("EDT" .  -400)
    ("AST" .  -400)			;by <clamen@CS.CMU.EDU>
    ("NST" .  -330)			;by <clamen@CS.CMU.EDU>
    ("UT"  .  +000)
    ("GMT" .  +000)
    ("BST" .  +100)
    ("MET" .  +100)
    ("EET" .  +200)
    ("JST" .  +900)
    ("GMT+1"  .  +100) ("GMT+2"  .  +200) ("GMT+3"  .  +300)
    ("GMT+4"  .  +400) ("GMT+5"  .  +500) ("GMT+6"  .  +600)
    ("GMT+7"  .  +700) ("GMT+8"  .  +800) ("GMT+9"  .  +900)
    ("GMT+10" . +1000) ("GMT+11" . +1100) ("GMT+12" . +1200) ("GMT+13" . +1300)
    ("GMT-1"  .  -100) ("GMT-2"  .  -200) ("GMT-3"  .  -300)
    ("GMT-4"  .  -400) ("GMT-5"  .  -500) ("GMT-6"  .  -600)
    ("GMT-7"  .  -700) ("GMT-8"  .  -800) ("GMT-9"  .  -900)
    ("GMT-10" . -1000) ("GMT-11" . -1100) ("GMT-12" . -1200))
  "*Time differentials of timezone from GMT in +-HHMM form.
This list is obsolescent, and is present only for backwards compatibility,
because time zone names are ambiguous in practice.
Use `current-time-zone' instead.")

(defvar timezone-months-assoc
  '(("JAN" .  1)("FEB" .  2)("MAR" .  3)
    ("APR" .  4)("MAY" .  5)("JUN" .  6)
    ("JUL" .  7)("AUG" .  8)("SEP" .  9)
    ("OCT" . 10)("NOV" . 11)("DEC" . 12))
  "Alist of first three letters of a month and its numerical representation.")

(defun timezone-make-date-arpa-standard (date &optional local timezone)
  "Convert DATE to an arpanet standard date.
Optional 2nd argument LOCAL specifies the default local timezone of the DATE;
if nil, GMT is assumed.
Optional 3rd argument TIMEZONE specifies a time zone to be represented in;
if nil, the local time zone is assumed."
  (let ((new (timezone-fix-time date local timezone)))
    (timezone-make-arpa-date (aref new 0) (aref new 1) (aref new 2)
			     (timezone-make-time-string
			      (aref new 3) (aref new 4) (aref new 5))
			     (aref new 6))
    ))

(defun timezone-make-date-sortable (date &optional local timezone)
  "Convert DATE to a sortable date string.
Optional 2nd argument LOCAL specifies the default local timezone of the DATE;
if nil, GMT is assumed.
Optional 3rd argument TIMEZONE specifies a timezone to be represented in;
if nil, the local time zone is assumed."
  (let ((new (timezone-fix-time date local timezone)))
    (timezone-make-sortable-date (aref new 0) (aref new 1) (aref new 2)
				 (timezone-make-time-string
				  (aref new 3) (aref new 4) (aref new 5)))
    ))


;;
;; Parsers and Constructors of Date and Time
;;

(defun timezone-make-arpa-date (year month day time &optional timezone)
  "Make arpanet standard date string from YEAR, MONTH, DAY, and TIME.
Optional argument TIMEZONE specifies a time zone."
  (let ((zone
	 (if (listp timezone)
	     (let* ((m (timezone-zone-to-minute timezone))
		    (absm (if (< m 0) (- m) m)))
	       (format "%c%02d%02d"
		       (if (< m 0) ?- ?+) (/ absm 60) (% absm 60)))
	   timezone)))
    (format "%02d %s %04d %s %s"
	    day
	    (capitalize (car (rassq month timezone-months-assoc)))
	    year
	    time
	    zone)))

(defun timezone-make-sortable-date (year month day time)
  "Make sortable date string from YEAR, MONTH, DAY, and TIME."
  (format "%4d%02d%02d%s"
	  year month day time))

(defun timezone-make-time-string (hour minute second)
  "Make time string from HOUR, MINUTE, and SECOND."
  (format "%02d:%02d:%02d" hour minute second))

(defun timezone-parse-date (date)
  "Parse DATE and return a vector [YEAR MONTH DAY TIME TIMEZONE].
Two-digit dates are `windowed'.  Those <69 have 2000 added; otherwise 1900
is added.  Three-digit dates have 1900 added.
TIMEZONE is nil for DATEs without a zone field.

Understands the following styles:
 (1) 14 Apr 89 03:20[:12] [GMT]
 (2) Fri, 17 Mar 89 4:01[:33] [GMT]
 (3) Mon Jan 16 16:12[:37] [GMT] 1989
 (4) 6 May 1992 1641-JST (Wednesday)
 (5) 22-AUG-1993 10:59:12.82
 (6) Thu, 11 Apr 16:17:12 91 [MET]
 (7) Mon, 6  Jul 16:47:20 T 1992 [MET]
 (8) 1996-06-24 21:13:12 [GMT]
 (9) 1996-06-24 21:13-ZONE
 (10) 19960624T211312"
  ;; Get rid of any text properties.
  (and (stringp date)
       (or (text-properties-at 0 date)
           (next-property-change 0 date))
       (setq date (copy-sequence date))
       (set-text-properties 0 (length date) nil date))
  (let ((date (or date ""))
        (year nil)
        (month nil)
        (day nil)
        (time nil)
        (zone nil))			;This may be nil.
    (cond ((string-match
            "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)" date)
           ;; Styles: (1) and (2) with timezone and buggy timezone
           ;; This is most common in mail and news,
           ;; so it is worth trying first.
           (setq year 3 month 2 day 1 time 4 zone 5))
          ((string-match
            "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]*\\'" date)
           ;; Styles: (1) and (2) without timezone
           (setq year 3 month 2 day 1 time 4 zone nil))
          ((string-match
            "\\([^ \t,]+\\),[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\(T[ \t]+\\|\\)\\([0-9]+\\)[ \t]*\\'" date)
           ;; Styles: (6) and (7) without timezone
           (setq year 6 month 3 day 2 time 4 zone nil))
          ((string-match
            "\\([^ \t,]+\\),[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\(T[ \t]+\\|\\)\\([0-9]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
           ;; Styles: (6) and (7) with timezone and buggy timezone
           (setq year 6 month 3 day 2 time 4 zone 7))
          ((string-match
            "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([0-9]+\\)" date)
           ;; Styles: (3) without timezone
           (setq year 4 month 1 day 2 time 3 zone nil))
          ((string-match
            "\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9:]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)[ \t]+\\([0-9]+\\)" date)
           ;; Styles: (3) with timezone
           (setq year 5 month 1 day 2 time 3 zone 4))
          ((string-match
            "\\([0-9]+\\)[ \t]+\\([^ \t,]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]*\\([-+a-zA-Z0-9]+\\)" date)
           ;; Styles: (4) with timezone
           (setq year 3 month 2 day 1 time 4 zone 5))
          ((string-match
            "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)\\(\\.[0-9]+\\)?[ \t]+\\([-+a-zA-Z0-9]+\\)" date)
           ;; Styles: (5) with timezone.
           (setq year 3 month 2 day 1 time 4 zone 6))
          ((string-match
            "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)\\(\\.[0-9]+\\)?" date)
           ;; Styles: (5) without timezone.
           (setq year 3 month 2 day 1 time 4 zone nil))
          ((string-match
            "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+\\([-+a-zA-Z0-9]+\\)" date)
           ;; Styles: (8) with timezone.
           (setq year 1 month 2 day 3 time 4 zone 5))
          ((string-match
            "\\([0-9]\\{4\\}\\)-?\\([0-9]\\{0,2\\}\\)-?\\([0-9]\\{0,2\\}\\)[T \t]+\\([0-9]\\{0,2\\}:?[0-9]\\{0,2\\}:?[0-9]\\{0,2\\}\\)[ \t]*\\([-+a-zA-Z]+[0-9:]*\\)" date)
           ;; Styles: (8) with timezone with a colon in it.
           (setq year 1 month 2 day 3 time 4 zone 5))
          ((string-match
            "\\([0-9]\\{4\\}\\)-?\\([0-9]\\{0,2\\}\\)-?\\([0-9]\\{0,2\\}\\)[T \t]+\\([0-9]+:?[0-9]+:?[0-9]+\\)" date)
           ;; Styles: (8) without timezone.
           (setq year 1 month 2 day 3 time 4 zone nil))
          )

    (when year
      (setq year (match-string year date))
      ;; Guess ambiguous years.  Assume years < 69 don't predate the
      ;; Unix Epoch, so are 2000+.  Three-digit years are assumed to
      ;; be relative to 1900.
      (when (< (length year) 4)
        (let ((y (string-to-number year)))
          (when (< y 69)
            (setq y (+ y 100)))
          (setq year (int-to-string (+ 1900 y)))))
      (setq month
            (if (or (= (aref date (+ (match-beginning month) 2)) ?-)
                    (let ((n (string-to-number
                              (char-to-string
                               (aref date (+ (match-beginning month) 2))))))
                      (= (aref (number-to-string n) 0)
                         (aref date (+ (match-beginning month) 2)))))
                ;; Handle numeric months, spanning exactly two digits.
                (substring date
                           (match-beginning month)
                           (+ (match-beginning month) 2))
              (let* ((string (substring date
                                        (match-beginning month)
                                        (+ (match-beginning month) 3)))
                     (monthnum
                      (cdr (assoc (upcase string) timezone-months-assoc))))
                (when monthnum
                  (int-to-string monthnum)))))
      (setq day (match-string day date))
      (setq time (match-string time date)))
    (when zone (setq zone (match-string zone date)))
    ;; Return a vector.
    (if (and year month)
        (vector year month day time zone)
      (vector "0" "0" "0" "0" nil))))

(defun timezone-parse-time (time)
  "Parse TIME (HH:MM:SS) and return a vector [hour minute second].
Recognize HH:MM:SS, HH:MM, HHMMSS, HHMM."
  (let ((time (or time ""))
	hour minute second)
    (cond ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\'" time)
	   ;; HH:MM:SS
	   (setq hour 1 minute 2 second 3))
	  ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" time)
	   ;; HH:MM
	   (setq hour 1 minute 2 second nil))
	  ((string-match "\\`\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\'" time)
	   ;; HHMMSS
	   (setq hour 1 minute 2 second 3))
	  ((string-match "\\`\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\'" time)
	   ;; HHMM
	   (setq hour 1 minute 2 second nil))
	  )
    ;; Return [hour minute second]
    (vector
     (if hour (match-string hour time) "0")
     (if minute (match-string minute time) "0")
     (if second (match-string second time) "0"))))


;; Miscellaneous

(defun timezone-zone-to-minute (timezone)
  "Translate TIMEZONE to an integer minute offset from GMT.
TIMEZONE can be a cons cell containing the output of `current-time-zone',
or an integer of the form +-HHMM, or a time zone name."
  (cond
     ((consp timezone)
      (/ (car timezone) 60))
     (timezone
      (progn
	(setq timezone
	      (or (cdr (assoc (upcase timezone) timezone-world-timezones))
		  ;; +900
		  timezone))
	(if (stringp timezone)
	    (setq timezone (string-to-number timezone)))
	;; Taking account of minute in timezone.
	;; HHMM -> MM
	(let* ((abszone (abs timezone))
 	       (minutes (+ (* 60 (/ abszone 100)) (% abszone 100))))
 	  (if (< timezone 0) (- minutes) minutes))))
     (t 0)))

(defun timezone-time-from-absolute (date seconds)
  "Compute the UTC time equivalent to DATE at time SECONDS after midnight.
Return a list suitable as an argument to `current-time-zone',
or nil if the date cannot be thus represented.
DATE is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((current-time-origin 719163)
	    ;; (timezone-absolute-from-gregorian 1 1 1970)
	 (days (- date current-time-origin))
	 (seconds-per-day (float 86400))
	 (seconds (+ seconds (* days seconds-per-day)))
	 (current-time-arithmetic-base (float 65536))
	 (hi (floor (/ seconds current-time-arithmetic-base)))
	 (hibase (* hi current-time-arithmetic-base))
	 (lo (floor (- seconds hibase))))
     (and (< (abs (- seconds (+ hibase lo))) 2) ;; Check for integer overflow.
	  (cons hi lo))))

(defun timezone-time-zone-from-absolute (date seconds)
  "Compute the local time zone for DATE at time SECONDS after midnight.
Return a list in the same format as `current-time-zone's result,
or nil if the local time zone could not be computed.
DATE is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
   (and (fboundp 'current-time-zone)
	(let ((utc-time (timezone-time-from-absolute date seconds)))
	  (and utc-time
	       (let ((zone (current-time-zone utc-time)))
		 (and (car zone) zone))))))

(defun timezone-fix-time (date local timezone)
  "Convert DATE (default timezone LOCAL) to YYYY-MM-DD-HH-MM-SS-ZONE vector.
If LOCAL is nil, it is assumed to be GMT.
If TIMEZONE is nil, use the local time zone."
  (let* ((date   (timezone-parse-date date))
	 (year   (string-to-number (aref date 0)))
	 (year	 (cond ((< year 69)
			(+ year 2000))
		       ((< year 100)
			(+ year 1900))
		       ((< year 1000)	; possible 3-digit years.
			(+ year 1900))
		       (t year)))
	 (month  (string-to-number (aref date 1)))
	 (day    (string-to-number (aref date 2)))
	 (time   (timezone-parse-time (aref date 3)))
	 (hour   (string-to-number (aref time 0)))
	 (minute (string-to-number (aref time 1)))
	 (second (string-to-number (aref time 2)))
	 (local  (or (aref date 4) local)) ;Use original if defined
	 (timezone
	  (or timezone
	      (timezone-time-zone-from-absolute
	       (timezone-absolute-from-gregorian month day year)
	       (+ second (* 60 (+ minute (* 60 hour)))))))
	 (diff   (- (timezone-zone-to-minute timezone)
		    (timezone-zone-to-minute local)))
	 (minute (+ minute diff))
	 (hour-fix (floor minute 60)))
    (setq hour (+ hour hour-fix))
    (setq minute (- minute (* 60 hour-fix)))
    ;; HOUR may be larger than 24 or smaller than 0.
    (cond ((<= 24 hour)			;24 -> 00
	   (setq hour (- hour 24))
	   (setq day  (1+ day))
	   (when (< (timezone-last-day-of-month month year) day)
	     (setq month (1+ month))
	     (setq day 1)
	     (when (< 12 month)
	       (setq month 1)
	       (setq year (1+ year)))))
	  ((> 0 hour)
	   (setq hour (+ hour 24))
	   (setq day  (1- day))
	   (when (> 1 day)
	     (setq month (1- month))
	     (when (> 1 month)
	       (setq month 12)
	       (setq year (1- year)))
	     (setq day (timezone-last-day-of-month month year)))))
    (vector year month day hour minute second timezone)))

;; Partly copied from Calendar program by Edward M. Reingold.
;; Thanks a lot.

(defun timezone-last-day-of-month (month year)
  "The last day in MONTH during YEAR."
  (if (and (= month 2) (timezone-leap-year-p year))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

(defun timezone-leap-year-p (year)
  "Return t if YEAR is a Gregorian leap year."
  (or (and (zerop  (% year 4))
	   (not (zerop (% year 100))))
      (zerop (% year 400))))

(defun timezone-day-number (month day year)
  "Return the day number within the year of the date month/day/year."
  (let ((day-of-year (+ day (* 31 (1- month)))))
    (if (> month 2)
	(progn
	  (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
	  (if (timezone-leap-year-p year)
	      (setq day-of-year (1+ day-of-year)))))
    day-of-year))

(defun timezone-absolute-from-gregorian (month day year)
  "The number of days between the Gregorian date 12/31/1 BC and month/day/year.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (+ (timezone-day-number month day year);; Days this year
     (* 365 (1- year));;	+ Days in prior years
     (/ (1- year) 4);;		+ Julian leap years
     (- (/ (1- year) 100));;	- century years
     (/ (1- year) 400)));;	+ Gregorian leap years

(provide 'timezone)

;;; timezone.el ends here
