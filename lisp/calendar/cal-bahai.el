;;; cal-bahai.el --- calendar functions for the Bahá'í calendar.

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Bahá'í calendar, Bahá'í, Baha'i, Bahai, calendar, diary
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

;; This collection of functions implements the features of calendar.el
;; and diary-lib.el that deal with the Bahá'í calendar.

;; The Bahá'í (http://www.bahai.org) calendar system is based on a
;; solar cycle of 19 months with 19 days each.  The four remaining
;; "intercalary" days are called the Ayyám-i-Há (days of Há), and are
;; placed between the 18th and 19th months.  They are meant as a time
;; of festivals preceding the 19th month, which is the month of
;; fasting.  In Gregorian leap years, there are 5 of these days (Há
;; has the numerical value of 5 in the arabic abjad, or
;; letter-to-number, reckoning).

;; Each month is named after an attribute of God, as are the 19 days
;; -- which have the same names as the months.  There is also a name
;; for each year in every 19 year cycle.  These cycles are called
;; Váhids.  A cycle of 19 Váhids (361 years) is called a Kullu-Shay,
;; which means "all things".

;; The calendar was named the "Badí` calendar" by its author, the Báb.
;; It uses a week of seven days, corresponding to the Gregorian week,
;; each of which has its own name, again patterned after the
;; attributes of God.

;; Note: The days of Ayyám-i-Há are encoded as zero and negative
;; offsets from the first day of the final month.  So, (19 -3 157) is
;; the first day of Ayyám-i-Há, in the year 157 BE.

;;; Code:

(require 'calendar)

(defconst calendar-bahai-month-name-array
  ["Bahá" "Jalál" "Jamál" "`Azamat" "Núr" "Rahmat" "Kalimát" "Kamál"
   "Asmá" "`Izzat" "Mashiyyat" "`Ilm" "Qudrat" "Qawl" "Masá'il"
   "Sharaf" "Sultán" "Mulk" "`Alá"]
  "Array of the month names in the Bahá'í calendar.")

(defconst calendar-bahai-epoch (calendar-absolute-from-gregorian '(3 21 1844))
  "Absolute date of start of Bahá'í calendar = March 21, 1844 AD.")

(defun calendar-bahai-leap-year-p (year)
  "True if Bahá'í YEAR is a leap year in the Bahá'í calendar."
  (calendar-leap-year-p (+ year 1844)))

(defconst calendar-bahai-leap-base
  (+ (/ 1844 4) (- (/ 1844 100)) (/ 1844 400))
  "Number of leap years between 1 and 1844 AD, inclusive.
Used by `calendar-bahai-to-absolute'.")

(defun calendar-bahai-to-absolute (date)
  "Compute absolute date from Bahá'í date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (prior-years (+ (1- year) 1844))
         (leap-days (- (+ (/ prior-years 4) ; leap days in prior years
                          (- (/ prior-years 100))
                          (/ prior-years 400))
                       calendar-bahai-leap-base)))
    (+ (1- calendar-bahai-epoch)        ; days before epoch
       (* 365 (1- year))                ; days in prior years
       leap-days
       (calendar-sum m 1 (< m month) 19)
       (if (= month 19)
           (if (calendar-bahai-leap-year-p year) 5 4)
         0)
       day)))                           ; days so far this month

(define-obsolete-function-alias 'calendar-absolute-from-bahai
  'calendar-bahai-to-absolute "23.1")

(defun calendar-bahai-from-absolute (date)
  "Bahá'í date (month day year) corresponding to the absolute DATE."
  (if (< date calendar-bahai-epoch)
      (list 0 0 0)                      ; pre-Bahá'í date
    (let* ((greg (calendar-gregorian-from-absolute date))
           (gmonth (calendar-extract-month greg))
           (year (+ (- (calendar-extract-year greg) 1844)
                    (if (or (> gmonth 3)
                            (and (= gmonth 3)
                                 (>= (calendar-extract-day greg) 21)))
                        1 0)))
           (month                       ; search forward from Baha
            (1+ (calendar-sum m 1
                  (> date (calendar-bahai-to-absolute (list m 19 year)))
                  1)))
           (day                     ; calculate the day by subtraction
            (- date
               (1- (calendar-bahai-to-absolute (list month 1 year))))))
      (list month day year))))

;;;###cal-autoload
(defun calendar-bahai-date-string (&optional date)
  "String of Bahá'í date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((bahai-date (calendar-bahai-from-absolute
                      (calendar-absolute-from-gregorian
                       (or date (calendar-current-date)))))
         (y (calendar-extract-year bahai-date)))
    (if (< y 1)
        ""                              ; pre-Bahai
      (let* ((m (calendar-extract-month bahai-date))
             (d (calendar-extract-day bahai-date))
             (monthname (if (and (= m 19)
                                 (<= d 0))
                            "Ayyám-i-Há"
                          (aref calendar-bahai-month-name-array (1- m))))
             (day (number-to-string
                   (if (<= d 0)
                       (+ d (if (calendar-bahai-leap-year-p y) 5 4))
                     d)))
             (year (number-to-string y))
             (month (number-to-string m))
             dayname)
        ;; Can't call calendar-date-string because of monthname oddity.
        (mapconcat 'eval calendar-date-display-form "")))))

;;;###cal-autoload
(defun calendar-bahai-print-date ()
  "Show the Bahá'í calendar equivalent of the selected date."
  (interactive)
  (let ((s (calendar-bahai-date-string (calendar-cursor-to-date t))))
   (if (string-equal s "")
       (message "Date is pre-Bahá'í")
     (message "Bahá'í date: %s" s))))

(define-obsolete-function-alias
  'calendar-print-bahai-date 'calendar-bahai-print-date "23.1")

(defun calendar-bahai-read-date ()
 "Interactively read the arguments for a Bahá'í date command.
Reads a year, month and day."
  (let* ((today (calendar-current-date))
         (year (calendar-read
                "Bahá'í calendar year (not 0): "
                (lambda (x) (not (zerop x)))
                (number-to-string
                 (calendar-extract-year
                  (calendar-bahai-from-absolute
                   (calendar-absolute-from-gregorian today))))))
         (completion-ignore-case t)
         (month (cdr (assoc
                      (completing-read
                       "Bahá'í calendar month name: "
                       (mapcar 'list
                               (append calendar-bahai-month-name-array nil))
                       nil t)
                      (calendar-make-alist calendar-bahai-month-name-array
                                           1))))
         (day (calendar-read "Bahá'í calendar day (1-19): "
                             (lambda (x) (and (< 0 x) (<= x 19))))))
    (list (list month day year))))

(define-obsolete-function-alias
  'calendar-bahai-prompt-for-date 'calendar-bahai-read-date "23.1")

;;;###cal-autoload
(defun calendar-bahai-goto-date (date &optional noecho)
  "Move cursor to Bahá'í date DATE; echo Bahá'í date unless NOECHO is non-nil."
  (interactive (calendar-bahai-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-bahai-to-absolute date)))
  (or noecho (calendar-bahai-print-date)))

(define-obsolete-function-alias
  'calendar-goto-bahai-date 'calendar-bahai-goto-date "23.1")

(defvar displayed-month)
(defvar displayed-year)

;;;###holiday-autoload
(defun holiday-bahai (month day string)
  "Holiday on MONTH, DAY (Bahá'í) called STRING.
If MONTH, DAY (Bahá'í) is visible in the current calendar window,
returns the corresponding Gregorian date in the form of the
list (((month day year) STRING)).  Otherwise, returns nil."
  ;; Since the calendar window shows 3 months at a time, there are
  ;; approx +/- 45 days either side of the central month.
  ;; Since the Bahai months have 19 days, this means up to +/- 3 months.
  (let* ((bahai-date (calendar-bahai-from-absolute
                      (calendar-absolute-from-gregorian
                       (list displayed-month 15 displayed-year))))
         (m (calendar-extract-month bahai-date))
         (y (calendar-extract-year bahai-date))
         date)
    (unless (< m 1)                    ; Bahá'í calendar doesn't apply
      ;; Cf holiday-fixed, holiday-islamic.
      ;; With a +- 3 month calendar window, and 19 months per year,
      ;; month 16 is special.  When m16 is central is when the
      ;; end-of-year first appears.  When m1 is central, m16 is no
      ;; longer visible.  Hence we can do a one-sided test to see if
      ;; m16 is visible.  m16 is visible when the central month >= 13.
      ;; To see if other months are visible we can shift the range
      ;; accordingly.
      (calendar-increment-month m y (- 16 month) 19)
      (and (> m 12)                     ; Bahá'í date might be visible
           (calendar-date-is-visible-p
            (setq date (calendar-gregorian-from-absolute
                        (calendar-bahai-to-absolute (list month day y)))))
           (list (list date string))))))

(autoload 'holiday-fixed "holidays")

;;;###holiday-autoload
(defun holiday-bahai-new-year ()
  "Holiday entry for the Bahá'í New Year, if visible in the calendar window."
  (holiday-fixed 3 21
                 (format "Bahá'í New Year (Naw-Ruz) %d"
                         (- displayed-year (1- 1844)))))

;;;###holiday-autoload
(defun holiday-bahai-ridvan (&optional all)
  "Holidays related to Ridvan, as visible in the calendar window.
Only considers the first, ninth, and twelfth days, unless ALL or
`calendar-bahai-all-holidays-flag' is non-nil."
  (let ((ord ["First" "Second" "Third" "Fourth" "Fifth" "Sixth"
              "Seventh" "Eighth" "Ninth" "Tenth" "Eleventh" "Twelfth"])
        (show '(0 8 11))
        rid h)
    (if (or all calendar-bahai-all-holidays-flag)
        (setq show (number-sequence 0 11)))
    ;; More trouble than it was worth...?
    (dolist (i show (nreverse rid))
      (if (setq h (holiday-fixed (if (< i 10) 4 5)
                                 (+ i (if (< i 10) 21 -9))
                                 (format "%s Day of Ridvan" (aref ord i))))
          (push (car h) rid)))))

(autoload 'diary-list-entries-1 "diary-lib")

;;;###diary-autoload
(defun diary-bahai-list-entries ()
  "Add any Bahá'í date entries from the diary file to `diary-entries-list'.
Bahá'í date diary entries must be prefaced by `diary-bahai-entry-symbol'
\(normally a `B').  The same diary date forms govern the style of the
Bahá'í calendar entries, except that the Bahá'í month names cannot be
abbreviated.  The Bahá'í months are numbered from 1 to 19 with Bahá being
1 and 19 being `Alá.  If a Bahá'í date diary entry begins with
`diary-nonmarking-symbol', the entry will appear in the diary listing, but
will not be marked in the calendar.  This function is provided for use with
`diary-nongregorian-listing-hook'."
  (diary-list-entries-1 calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))
(define-obsolete-function-alias
  'list-bahai-diary-entries 'diary-bahai-list-entries "23.1")


(autoload 'calendar-mark-1 "diary-lib")

;;;###diary-autoload
(defun calendar-bahai-mark-date-pattern (month day year &optional color)
  "Mark dates in calendar window that conform to Bahá'í date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `calendar-mark-visible-date' as MARK."
  (calendar-mark-1 month day year 'calendar-bahai-from-absolute
                   'calendar-bahai-to-absolute color))

(define-obsolete-function-alias
  'mark-bahai-calendar-date-pattern 'calendar-bahai-mark-date-pattern "23.1")


(autoload 'diary-mark-entries-1 "diary-lib")

;;;###diary-autoload
(defun diary-bahai-mark-entries ()
  "Mark days in the calendar window that have Bahá'í date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `diary-bahai-list-entries' for more information."
  (diary-mark-entries-1 'calendar-bahai-mark-date-pattern
                        calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

(define-obsolete-function-alias
  'mark-bahai-diary-entries 'diary-bahai-mark-entries "23.1")


(autoload 'diary-insert-entry-1 "diary-lib")

;;;###cal-autoload
(defun diary-bahai-insert-entry (arg)
  "Insert a diary entry.
For the Bahá'í date corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 nil arg calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

(define-obsolete-function-alias
  'insert-bahai-diary-entry 'diary-bahai-insert-entry "23.1")

;;;###cal-autoload
(defun diary-bahai-insert-monthly-entry (arg)
  "Insert a monthly diary entry.
For the day of the Bahá'í month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'monthly arg calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

(define-obsolete-function-alias
  'insert-monthly-bahai-diary-entry 'diary-bahai-insert-monthly-entry "23.1")

;;;###cal-autoload
(defun diary-bahai-insert-yearly-entry (arg)
  "Insert an annual diary entry.
For the day of the Bahá'í year corresponding to the date indicated by point.
Prefix argument ARG will make the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'yearly arg calendar-bahai-month-name-array
                        diary-bahai-entry-symbol
                        'calendar-bahai-from-absolute))

(define-obsolete-function-alias
  'insert-yearly-bahai-diary-entry 'diary-bahai-insert-yearly-entry "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-bahai-date ()
  "Bahá'í calendar equivalent of date diary entry."
  (format "Bahá'í date: %s" (calendar-bahai-date-string date)))


(provide 'cal-bahai)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cal-bahai.el ends here
