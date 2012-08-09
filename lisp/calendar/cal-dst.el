;;; cal-dst.el --- calendar functions for daylight saving rules

;; Copyright (C) 1993-1996, 2001-2012  Free Software Foundation, Inc.

;; Author: Paul Eggert <eggert@twinsun.com>
;;         Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: daylight saving time, calendar, diary, holidays
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

;; See calendar.el.

;;; Code:

(require 'calendar)


(defgroup calendar-dst nil
  "Options related to Daylight Saving Time."
  :prefix "calendar-"
  :group 'calendar)


(defcustom calendar-dst-check-each-year-flag t
  "Non-nil means to check each year for DST transitions as needed.
Otherwise assume the next two transitions found after the
current date apply to all years.  This is faster, but not always
correct, since the dates of daylight saving transitions sometimes
change."
  :type 'boolean
  :version "22.1"
  :group 'calendar-dst)

;;;###autoload
(put 'calendar-daylight-savings-starts 'risky-local-variable t)
(defcustom calendar-daylight-savings-starts '(calendar-dst-starts year)
  "Sexp giving the date on which daylight saving time starts.
This is an expression in the variable `year' whose value gives the Gregorian
date in the form (month day year) on which daylight saving time starts.  It is
used to determine the starting date of daylight saving time for the holiday
list and for correcting times of day in the solar and lunar calculations.

For example, if daylight saving time is mandated to start on October 1,
you would set `calendar-daylight-savings-starts' to

      '(10 1 year)

If it starts on the first Sunday in April, you would set it to

      '(calendar-nth-named-day 1 0 4 year)

If the locale never uses daylight saving time, set this to nil."
  :type 'sexp
  :group 'calendar-dst)

;;;###autoload
(put 'calendar-daylight-savings-ends 'risky-local-variable t)
(defcustom calendar-daylight-savings-ends '(calendar-dst-ends year)
  "Sexp giving the date on which daylight saving time ends.
This is an expression in the variable `year' whose value gives the Gregorian
date in the form (month day year) on which daylight saving time ends.  It is
used to determine the starting date of daylight saving time for the holiday
list and for correcting times of day in the solar and lunar calculations.

For example, if daylight saving time ends on the last Sunday in October:

      '(calendar-nth-named-day -1 0 10 year)

If the locale never uses daylight saving time, set this to nil."
  :type 'sexp
  :group 'calendar-dst)

;;; More defcustoms below.


(defvar calendar-current-time-zone-cache nil
  "Cache for result of `calendar-current-time-zone'.")
;; It gets eval'd, eg by calendar-dst-starts.
;;;###autoload
(put 'calendar-current-time-zone-cache 'risky-local-variable t)

(defvar calendar-system-time-basis
  (calendar-absolute-from-gregorian '(1 1 1970))
  "Absolute date of starting date of system clock.")

(defun calendar-absolute-from-time (x utc-diff)
  "Absolute local date of time X; local time is UTC-DIFF seconds from UTC.

X is (HIGH . LOW) or (HIGH LOW . IGNORED) where HIGH and LOW are the
high and low 16 bits, respectively, of the number of seconds since
1970-01-01 00:00:00 UTC, ignoring leap seconds.

Returns the pair (ABS-DATE . SECONDS) where SECONDS after local midnight on
absolute date ABS-DATE is the equivalent moment to X."
  (let* ((h (car x))
         (xtail (cdr x))
         (l (+ utc-diff (if (numberp xtail) xtail (car xtail))))
         (u (+ (* 512 (mod h 675)) (floor l 128))))
    ;; Overflow is a terrible thing!
    (cons (+ calendar-system-time-basis
             ;; floor((2^16 h +l) / (60*60*24))
             (* 512 (floor h 675)) (floor u 675))
          ;; (2^16 h +l) mod (60*60*24)
          (+ (* (mod u 675) 128) (mod l 128)))))

(defun calendar-time-from-absolute (abs-date s)
  "Time of absolute date ABS-DATE, S seconds after midnight.

Returns the list (HIGH LOW) where HIGH and LOW are the high and low
16 bits, respectively, of the number of seconds 1970-01-01 00:00:00 UTC,
ignoring leap seconds, that is the equivalent moment to S seconds after
midnight UTC on absolute date ABS-DATE."
  (let* ((a (- abs-date calendar-system-time-basis))
         (u (+ (* 163 (mod a 512)) (floor s 128))))
    ;; Overflow is a terrible thing!
    (list
     ;; floor((60*60*24*a + s) / 2^16)
     (+ a (* 163 (floor a 512)) (floor u 512))
     ;; (60*60*24*a + s) mod 2^16
     (+ (* 128 (mod u 512)) (mod s 128)))))

(defun calendar-next-time-zone-transition (time)
  "Return the time of the next time zone transition after TIME.
Both TIME and the result are acceptable arguments to `current-time-zone'.
Return nil if no such transition can be found."
  (let* ((base 65536)           ; 2^16 = base of current-time output
         (quarter-multiple 120) ; approx = (seconds per quarter year) / base
         (time-zone (current-time-zone time))
         (time-utc-diff (car time-zone))
         hi
         hi-zone
         (hi-utc-diff time-utc-diff)
         (quarters '(2 1 3)))
    ;; Heuristic: probe the time zone offset in the next three calendar
    ;; quarters, looking for a time zone offset different from TIME.
    (while (and quarters (eq time-utc-diff hi-utc-diff))
      (setq hi (cons (+ (car time) (* (car quarters) quarter-multiple)) 0)
            hi-zone (current-time-zone hi)
            hi-utc-diff (car hi-zone)
            quarters (cdr quarters)))
    (and
     time-utc-diff
     hi-utc-diff
     (not (eq time-utc-diff hi-utc-diff))
     ;; Now HI is after the next time zone transition.
     ;; Set LO to TIME, and then binary search to increase LO and decrease HI
     ;; until LO is just before and HI is just after the time zone transition.
     (let* ((tail (cdr time))
            (lo (cons (car time) (if (numberp tail) tail (car tail))))
            probe)
       (while
           ;; Set PROBE to halfway between LO and HI, rounding down.
           ;; If PROBE equals LO, we are done.
           (let* ((lsum (+ (cdr lo) (cdr hi)))
                  (hsum (+ (car lo) (car hi) (/ lsum base)))
                  (hsumodd (logand 1 hsum)))
             (setq probe (cons (/ (- hsum hsumodd) 2)
                               (/ (+ (* hsumodd base) (% lsum base)) 2)))
             (not (equal lo probe)))
         ;; Set either LO or HI to PROBE, depending on probe results.
         (if (eq (car (current-time-zone probe)) hi-utc-diff)
             (setq hi probe)
           (setq lo probe)))
       hi))))

(autoload 'calendar-persian-to-absolute "cal-persia")

(defun calendar-time-zone-daylight-rules (abs-date utc-diff)
  "Return daylight transition rule for ABS-DATE, UTC-DIFF sec offset from UTC.
ABS-DATE must specify a day that contains a daylight saving transition.
The result has the proper form for `calendar-daylight-savings-starts'."
  (let* ((date (calendar-gregorian-from-absolute abs-date))
         (weekday (% abs-date 7))
         (m (calendar-extract-month date))
         (d (calendar-extract-day date))
         (y (calendar-extract-year date))
         (last (calendar-last-day-of-month m y))
         j rlist
         (candidate-rules               ; these return Gregorian dates
          (append
           ;; Day D of month M.
           `((list ,m ,d year))
           ;; The first WEEKDAY of month M.
           (if (< d 8)
               `((calendar-nth-named-day 1 ,weekday ,m year)))
           ;; The last WEEKDAY of month M.
           (if (> d (- last 7))
               `((calendar-nth-named-day -1 ,weekday ,m year)))
           (progn
             ;; The first WEEKDAY after day J of month M, for D-6 < J <= D.
             (setq j (1- (max 2 (- d 6))))
             (while (<= (setq j (1+ j)) (min d (- last 8)))
               (push `(calendar-nth-named-day 1 ,weekday ,m year ,j) rlist))
             rlist)
           ;; 01-01 and 07-01 for this year's Persian calendar.
           ;; FIXME what does the Persian calendar have to do with this?
           (and (= m 3) (memq d '(20 21))
                '((calendar-gregorian-from-absolute
                   (calendar-persian-to-absolute `(1 1 ,(- year 621))))))
           (and (= m 9) (memq d '(22 23))
                '((calendar-gregorian-from-absolute
                   (calendar-persian-to-absolute `(7 1 ,(- year 621))))))))
         (prevday-sec (- -1 utc-diff)) ; last sec of previous local day
         (year (1+ y))
         new-rules)
    ;; Scan through the next few years until only one rule remains.
    (while (cdr candidate-rules)
      (dolist (rule candidate-rules)
        ;; The rule we return should give a Gregorian date, but here
        ;; we require an absolute date.  The following is for efficiency.
        (setq date (cond ((eq (car rule) 'calendar-nth-named-day)
                          (eval (cons 'calendar-nth-named-absday (cdr rule))))
                         ((eq (car rule) 'calendar-gregorian-from-absolute)
                          (eval (cadr rule)))
                         (t (calendar-absolute-from-gregorian (eval rule)))))
        (or (equal (current-time-zone
                    (calendar-time-from-absolute date prevday-sec))
                   (current-time-zone
                    (calendar-time-from-absolute (1+ date) prevday-sec)))
            (setq new-rules (cons rule new-rules))))
      ;; If no rules remain, just use the first candidate rule;
      ;; it's wrong in general, but it's right for at least one year.
      (setq candidate-rules (if new-rules (nreverse new-rules)
                              (list (car candidate-rules)))
            new-rules nil
            year (1+ year)))
    (car candidate-rules)))

;; TODO it might be better to extract this information directly from
;; the system timezone database. But cross-platform...?
;; See thread
;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2006-11/msg00060.html
(defun calendar-dst-find-data (&optional time)
  "Find data on the first daylight saving time transitions after TIME.
TIME defaults to `current-time'.  Return value is as described
for `calendar-current-time-zone'."
  (let* ((t0 (or time (current-time)))
         (t0-zone (current-time-zone t0))
         (t0-utc-diff (car t0-zone))
         (t0-name (cadr t0-zone)))
    (if (not t0-utc-diff)
        ;; Little or no time zone information is available.
        (list nil nil t0-name t0-name nil nil nil nil)
      (let* ((t1 (calendar-next-time-zone-transition t0))
             (t2 (and t1 (calendar-next-time-zone-transition t1))))
        (if (not t2)
            ;; This locale does not have daylight saving time.
            (list (/ t0-utc-diff 60) 0 t0-name t0-name nil nil 0 0)
          ;; Use heuristics to find daylight saving parameters.
          (let* ((t1-zone (current-time-zone t1))
                 (t1-utc-diff (car t1-zone))
                 (t1-name (cadr t1-zone))
                 (t1-date-sec (calendar-absolute-from-time t1 t0-utc-diff))
                 (t2-date-sec (calendar-absolute-from-time t2 t1-utc-diff))
                 ;; TODO When calendar-dst-check-each-year-flag is non-nil,
                 ;; the rules can be simpler than they currently are.
                 (t1-rules (calendar-time-zone-daylight-rules
                            (car t1-date-sec) t0-utc-diff))
                 (t2-rules (calendar-time-zone-daylight-rules
                            (car t2-date-sec) t1-utc-diff))
                 (t1-time (/ (cdr t1-date-sec) 60))
                 (t2-time (/ (cdr t2-date-sec) 60)))
            (cons
             (/ (min t0-utc-diff t1-utc-diff) 60)
             (cons
              (/ (abs (- t0-utc-diff t1-utc-diff)) 60)
              (if (< t0-utc-diff t1-utc-diff)
                  (list t0-name t1-name t1-rules t2-rules t1-time t2-time)
                (list t1-name t0-name t2-rules t1-rules t2-time t1-time)
                )))))))))

(defvar calendar-dst-transition-cache nil
  "Internal cal-dst variable storing date of daylight saving time transitions.
Value is a list with elements of the form (YEAR START END), where
START and END are expressions that when evaluated return the
start and end dates (respectively) for DST in YEAR.  Used by the
function `calendar-dst-find-startend'.")

(defun calendar-dst-find-startend (year)
  "Find the dates in YEAR on which daylight saving time starts and ends.
Returns a list (YEAR START END), where START and END are
expressions that when evaluated return the start and end dates,
respectively. This function first attempts to use pre-calculated
data from `calendar-dst-transition-cache', otherwise it calls
`calendar-dst-find-data' (and adds the results to the cache).
If dates in YEAR cannot be handled by `encode-time' (e.g. if they
are too large to be represented as a lisp integer), then rather
than an error this function returns the result appropriate for
the current year."
  (let ((e (assoc year calendar-dst-transition-cache))
        f)
    (or e
        (progn
          (setq e (calendar-dst-find-data
                   (condition-case nil
                       (encode-time 1 0 0 1 1 year)
                     (error
                      (encode-time 1 0 0 1 1 (nth 5 (decode-time))))))
                f (nth 4 e)
                e (list year f (nth 5 e))
                calendar-dst-transition-cache
                (append calendar-dst-transition-cache (list e)))
          e))))

(defun calendar-current-time-zone ()
  "Return UTC difference, dst offset, names and rules for current time zone.

Returns (UTC-DIFF DST-OFFSET STD-ZONE DST-ZONE DST-STARTS DST-ENDS
DST-STARTS-TIME DST-ENDS-TIME), based on a heuristic probing of what the
system knows:

UTC-DIFF is an integer specifying the number of minutes difference between
    standard time in the current time zone and Coordinated Universal Time
    (Greenwich Mean Time).  A negative value means west of Greenwich.
DST-OFFSET is an integer giving the daylight saving time offset in minutes.
STD-ZONE is a string giving the name of the time zone when no seasonal time
    adjustment is in effect.
DST-ZONE is a string giving the name of the time zone when there is a seasonal
    time adjustment in effect.
DST-STARTS and DST-ENDS are sexps in the variable `year' giving the daylight
    saving time start and end rules, in the form expected by
    `calendar-daylight-savings-starts'.
DST-STARTS-TIME and DST-ENDS-TIME are integers giving the number of minutes
    after midnight that daylight saving time starts and ends.

If the local area does not use a seasonal time adjustment, STD-ZONE and
DST-ZONE are equal, and all the DST-* integer variables are 0.

Some operating systems cannot provide all this information to Emacs; in this
case, `calendar-current-time-zone' returns a list containing nil for the data
it can't find."
  (unless calendar-current-time-zone-cache
    (setq calendar-current-time-zone-cache (calendar-dst-find-data))))


;; Following options should be set based on conditions when the code
;; is invoked, so are not suitable for dumping into loaddefs.el.  They
;; default to US Eastern time if time zone info is not available.

(calendar-current-time-zone)

(defcustom calendar-time-zone (or (car calendar-current-time-zone-cache) -300)
  "Number of minutes difference between local standard time and UTC.
For example, -300 for New York City, -480 for Los Angeles."
  :type 'integer
  :group 'calendar-dst)

(defcustom calendar-daylight-time-offset
  (or (cadr calendar-current-time-zone-cache) 60)
  "Number of minutes difference between daylight saving and standard time.
If the locale never uses daylight saving time, set this to 0."
  :type 'integer
  :group 'calendar-dst)

(defcustom calendar-standard-time-zone-name
  (or (nth 2 calendar-current-time-zone-cache) "EST")
  "Abbreviated name of standard time zone at `calendar-location-name'.
For example, \"EST\" in New York City, \"PST\" for Los Angeles."
  :type 'string
  :group 'calendar-dst)

(defcustom calendar-daylight-time-zone-name
  (or (nth 3 calendar-current-time-zone-cache) "EDT")
  "Abbreviated name of daylight saving time zone at `calendar-location-name'.
For example, \"EDT\" in New York City, \"PDT\" for Los Angeles."
  :type 'string
  :group 'calendar-dst)

(defcustom calendar-daylight-savings-starts-time
  (or (nth 6 calendar-current-time-zone-cache) 120)
  "Number of minutes after midnight that daylight saving time starts."
  :type 'integer
  :group 'calendar-dst)

(defcustom calendar-daylight-savings-ends-time
  (or (nth 7 calendar-current-time-zone-cache)
      calendar-daylight-savings-starts-time)
  "Number of minutes after midnight that daylight saving time ends."
  :type 'integer
  :group 'calendar-dst)


(defun calendar-dst-starts (year)
  "Return the date of YEAR on which daylight saving time starts.
This function respects the value of `calendar-dst-check-each-year-flag'."
  (or (let ((expr (if calendar-dst-check-each-year-flag
                      (cadr (calendar-dst-find-startend year))
                    (nth 4 calendar-current-time-zone-cache))))
        (if expr (eval expr)))
      ;; New US rules commencing 2007.  ftp://elsie.nci.nih.gov/pub/.
      (and (not (zerop calendar-daylight-time-offset))
           (calendar-nth-named-day 2 0 3 year))))

(defun calendar-dst-ends (year)
  "Return the date of YEAR on which daylight saving time ends.
This function respects the value of `calendar-dst-check-each-year-flag'."
  (or (let ((expr (if calendar-dst-check-each-year-flag
                      (nth 2 (calendar-dst-find-startend year))
                    (nth 5 calendar-current-time-zone-cache))))
        (if expr (eval expr)))
      ;; New US rules commencing 2007.  ftp://elsie.nci.nih.gov/pub/.
      (and (not (zerop calendar-daylight-time-offset))
           (calendar-nth-named-day 1 0 11 year))))

;; used by calc, solar.
(defun dst-in-effect (date)
  "True if on absolute DATE daylight saving time is in effect.
Fractional part of DATE is local standard time of day."
  (let* ((year (calendar-extract-year
                (calendar-gregorian-from-absolute (floor date))))
         (dst-starts-gregorian (eval calendar-daylight-savings-starts))
         (dst-ends-gregorian (eval calendar-daylight-savings-ends))
         (dst-starts (and dst-starts-gregorian
                          (+ (calendar-absolute-from-gregorian
                              dst-starts-gregorian)
                             (/ calendar-daylight-savings-starts-time
                                60.0 24.0))))
         (dst-ends (and dst-ends-gregorian
                        (+ (calendar-absolute-from-gregorian
                            dst-ends-gregorian)
                           (/ (- calendar-daylight-savings-ends-time
                                 calendar-daylight-time-offset)
                              60.0 24.0)))))
    (and dst-starts dst-ends
         (if (< dst-starts dst-ends)
             (and (<= dst-starts date) (< date dst-ends))
           (or (<= dst-starts date) (< date dst-ends))))))

;; used by calc, lunar, solar.
(defun dst-adjust-time (date time)
  "Adjust, to account for dst on DATE, decimal fraction standard TIME.
Returns a list (date adj-time zone) where `date' and `adj-time' are the values
adjusted for `zone'; here `date' is a list (month day year), `adj-time' is a
decimal fraction time, and `zone' is a string.

Conversion to daylight saving time is done according to
`calendar-daylight-savings-starts', `calendar-daylight-savings-ends',
`calendar-daylight-savings-starts-time',
`calendar-daylight-savings-ends-time', and `calendar-daylight-time-offset'."
  (let* ((rounded-abs-date (+ (calendar-absolute-from-gregorian date)
                              (/ (round (* 60 time)) 60.0 24.0)))
         (dst (dst-in-effect rounded-abs-date))
         (time-zone (if dst
                        calendar-daylight-time-zone-name
                      calendar-standard-time-zone-name))
         (time (+ rounded-abs-date
                  (if dst (/ calendar-daylight-time-offset 24.0 60.0) 0))))
    (list (calendar-gregorian-from-absolute (truncate time))
          (* 24.0 (- time (truncate time)))
          time-zone)))

(provide 'cal-dst)

;;; cal-dst.el ends here
