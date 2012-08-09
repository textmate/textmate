;;; cal-islam.el --- calendar functions for the Islamic calendar

;; Copyright (C) 1995, 1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Islamic calendar, calendar, diary
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

(defconst calendar-islamic-month-name-array
  ["Muharram" "Safar" "Rabi I" "Rabi II" "Jumada I" "Jumada II"
   "Rajab" "Sha'ban" "Ramadan" "Shawwal" "Dhu al-Qada" "Dhu al-Hijjah"]
"Array of strings giving the names of the Islamic months.")

(eval-and-compile
  (autoload 'calendar-julian-to-absolute "cal-julian"))

(defconst calendar-islamic-epoch
  (eval-when-compile (calendar-julian-to-absolute '(7 16 622)))
  "Absolute date of start of Islamic calendar = July 16, 622 AD (Julian).")

(defun calendar-islamic-leap-year-p (year)
  "Return t if YEAR is a leap year on the Islamic calendar."
  (memq (% year 30)
        (list 2 5 7 10 13 16 18 21 24 26 29)))

(defun calendar-islamic-last-day-of-month (month year)
  "The last day in MONTH during YEAR on the Islamic calendar."
  (cond
   ((memq month (list 1 3 5 7 9 11)) 30)
   ((memq month (list 2 4 6 8 10)) 29)
   (t (if (calendar-islamic-leap-year-p year) 30 29))))

(defun calendar-islamic-day-number (date)
  "Return the day number within the year of the Islamic date DATE."
  (let ((month (calendar-extract-month date)))
    (+ (* 30 (/ month 2))
       (* 29 (/ (1- month) 2))
       (calendar-extract-day date))))

(defun calendar-islamic-to-absolute (date)
  "Absolute date of Islamic DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (y (% year 30))
         (leap-years-in-cycle (cond ((< y 3) 0)
                                    ((< y 6) 1)
                                    ((< y 8) 2)
                                    ((< y 11) 3)
                                    ((< y 14) 4)
                                    ((< y 17) 5)
                                    ((< y 19) 6)
                                    ((< y 22) 7)
                                    ((< y 25) 8)
                                    ((< y 27) 9)
                                    (t 10))))
    (+ (calendar-islamic-day-number date) ; days so far this year
       (* (1- year) 354)                  ; days in all non-leap years
       (* 11 (/ year 30))             ; leap days in complete cycles
       leap-years-in-cycle            ; leap days this cycle
       (1- calendar-islamic-epoch)))) ; days before start of calendar

(define-obsolete-function-alias 'calendar-absolute-from-islamic
  'calendar-islamic-to-absolute "23.1")

(defun calendar-islamic-from-absolute (date)
  "Compute the Islamic date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (if (< date calendar-islamic-epoch)
      (list 0 0 0)                      ; pre-Islamic date
    (let* ((approx (/ (- date calendar-islamic-epoch)
                      355))  ; approximation from below
           (year             ; search forward from the approximation
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-islamic-to-absolute
                                       (list 1 1 (1+ y))))
                             1)))
           (month                       ; search forward from Muharram
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-islamic-to-absolute
                                  (list m
                                        (calendar-islamic-last-day-of-month
                                         m year)
                                        year)))
                              1)))
           (day                    ; calculate the day by subtraction
            (- date
               (1- (calendar-islamic-to-absolute (list month 1 year))))))
      (list month day year))))

;;;###cal-autoload
(defun calendar-islamic-date-string (&optional date)
  "String of Islamic date before sunset of Gregorian DATE.
Returns the empty string if DATE is pre-Islamic.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'."
  (let ((calendar-month-name-array calendar-islamic-month-name-array)
        (islamic-date (calendar-islamic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date))))))
    (if (< (calendar-extract-year islamic-date) 1)
        ""
      (calendar-date-string islamic-date nil t))))

;;;###cal-autoload
(defun calendar-islamic-print-date ()
  "Show the Islamic calendar equivalent of the date under the cursor."
  (interactive)
  (let ((i (calendar-islamic-date-string (calendar-cursor-to-date t))))
    (if (string-equal i "")
        (message "Date is pre-Islamic")
      (message "Islamic date (until sunset): %s" i))))

(define-obsolete-function-alias 'calendar-print-islamic-date
  'calendar-islamic-print-date "23.1")

(defun calendar-islamic-read-date ()
  "Interactively read the arguments for an Islamic date command.
Reads a year, month, and day."
  (let* ((today (calendar-current-date))
         (year (calendar-read
                "Islamic calendar year (>0): "
                (lambda (x) (> x 0))
                (number-to-string
                 (calendar-extract-year
                  (calendar-islamic-from-absolute
                   (calendar-absolute-from-gregorian today))))))
         (month-array calendar-islamic-month-name-array)
         (completion-ignore-case t)
         (month (cdr (assoc-string
                      (completing-read
                       "Islamic calendar month name: "
                       (mapcar 'list (append month-array nil))
                       nil t)
                      (calendar-make-alist month-array 1) t)))
         (last (calendar-islamic-last-day-of-month month year))
         (day (calendar-read
               (format "Islamic calendar day (1-%d): " last)
               (lambda (x) (and (< 0 x) (<= x last))))))
    (list (list month day year))))

;;;###cal-autoload
(defun calendar-islamic-goto-date (date &optional noecho)
  "Move cursor to Islamic DATE; echo Islamic date unless NOECHO is non-nil."
  (interactive (calendar-islamic-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-islamic-to-absolute date)))
  (or noecho (calendar-islamic-print-date)))

(define-obsolete-function-alias 'calendar-goto-islamic-date
  'calendar-islamic-goto-date "23.1")

(defvar displayed-month)                ; from calendar-generate
(defvar displayed-year)

;;;###holiday-autoload
(defun holiday-islamic (month day string)
  "Holiday on MONTH, DAY (Islamic) called STRING.
If MONTH, DAY (Islamic) is visible, returns the corresponding
Gregorian date as the list (((month day year) STRING)).
Returns nil if it is not visible in the current calendar window."
  ;; Islamic date corresponding to the center of the calendar window.
  ;; Since the calendar displays 3 months at a time, there are approx
  ;; 45 visible days either side of this date.  Given the length of
  ;; the Islamic months, this means up to two different months are
  ;; visible either side of the central date.
  (let* ((islamic-date (calendar-islamic-from-absolute
                        (calendar-absolute-from-gregorian
                         (list displayed-month 15 displayed-year))))
         (m (calendar-extract-month islamic-date))
         (y (calendar-extract-year islamic-date))
         date)
    (unless (< m 1)                   ; Islamic calendar doesn't apply
      ;; Since converting to absolute dates can be a complex
      ;; operation, we try to speed things up by excluding those date
      ;; ranges that can't possibly be visible.
      ;; We can view the situation (see above) as if we had a calendar
      ;; window displaying 5 months at a time.  When month m is
      ;; central, months m-2:m+2 (modulo 12) might be visible.
      ;; Recall from holiday-fixed that with a 3 month calendar
      ;; window, November is special, because we can do a one-sided
      ;; inclusion test.  When November is central is when the end of
      ;; year first appears on the calendar.  Similarly, with a 5
      ;; month window, October is special.  When October is central is
      ;; when the end of year first appears, and when January is
      ;; central, October is no longer visible.  October is visible
      ;; when the central month is >= 8.
      ;; Hence to test if any given month might be visible, we can
      ;; shift things and ask about October.
      ;; At the same time, we work out the appropriate year y to use.
      (calendar-increment-month m y (- 10 month))
      (and (> m 7)                      ; Islamic date might be visible
           (calendar-date-is-visible-p
            (setq date (calendar-gregorian-from-absolute
                        (calendar-islamic-to-absolute (list month day y)))))
           (list (list date string))))))

;;;###holiday-autoload
(defun holiday-islamic-new-year ()
  "Holiday entry for the Islamic New Year, if visible in the calendar window."
  (let ((date (caar (holiday-islamic 1 1 "")))
        (m displayed-month)
        (y displayed-year))
    (and date
         (list (list date
                     (format "Islamic New Year %d"
                             (progn
                               (calendar-increment-month m y 1)
                               (calendar-extract-year
                                (calendar-islamic-from-absolute
                                 (calendar-absolute-from-gregorian
                                  (list m (calendar-last-day-of-month m y) y)
                                  ))))))))))

(autoload 'diary-list-entries-1 "diary-lib")

;;;###diary-autoload
(defun diary-islamic-list-entries ()
  "Add any Islamic date entries from the diary file to `diary-entries-list'.
Islamic date diary entries must be prefaced by `diary-islamic-entry-symbol'
\(normally an `I').  The same `diary-date-forms' govern the style
of the Islamic calendar entries, except that the Islamic month
names cannot be abbreviated.  The Islamic months are numbered
from 1 to 12 with Muharram being 1 and 12 being Dhu al-Hijjah.
If an Islamic date diary entry begins with `diary-nonmarking-symbol',
the entry will appear in the diary listing, but will not be
marked in the calendar.  This function is provided for use with
`diary-nongregorian-listing-hook'."
  (diary-list-entries-1 calendar-islamic-month-name-array
                        diary-islamic-entry-symbol
                        'calendar-islamic-from-absolute))

(define-obsolete-function-alias 'list-islamic-diary-entries
  'diary-islamic-list-entries "23.1")

(autoload 'calendar-mark-1 "diary-lib")

;;;###diary-autoload
(defun calendar-islamic-mark-date-pattern (month day year &optional color)
  "Mark dates in calendar window that conform to Islamic date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `calendar-mark-visible-date' as MARK."
  (calendar-mark-1 month day year 'calendar-islamic-from-absolute
                   'calendar-islamic-to-absolute color))

(define-obsolete-function-alias 'mark-islamic-calendar-date-pattern
  'calendar-islamic-mark-date-pattern "23.1")

(autoload 'diary-mark-entries-1 "diary-lib")

;;;###diary-autoload
(defun diary-islamic-mark-entries ()
  "Mark days in the calendar window that have Islamic date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `diary-islamic-list-entries' for more information."
  (diary-mark-entries-1 'calendar-islamic-mark-date-pattern
                        calendar-islamic-month-name-array
                        diary-islamic-entry-symbol
                        'calendar-islamic-from-absolute))

(define-obsolete-function-alias
  'mark-islamic-diary-entries 'diary-islamic-mark-entries "23.1")

(autoload 'diary-insert-entry-1 "diary-lib")

;;;###cal-autoload
(defun diary-islamic-insert-entry (arg)
  "Insert a diary entry.
For the Islamic date corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 nil arg calendar-islamic-month-name-array
                        diary-islamic-entry-symbol
                        'calendar-islamic-from-absolute))

(define-obsolete-function-alias 'insert-islamic-diary-entry
  'diary-islamic-insert-entry "23.1")

;;;###cal-autoload
(defun diary-islamic-insert-monthly-entry (arg)
  "Insert a monthly diary entry.
For the day of the Islamic month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'monthly arg calendar-islamic-month-name-array
                        diary-islamic-entry-symbol
                        'calendar-islamic-from-absolute))

(define-obsolete-function-alias 'insert-monthly-islamic-diary-entry
  'diary-islamic-insert-monthly-entry "23.1")

;;;###cal-autoload
(defun diary-islamic-insert-yearly-entry (arg)
  "Insert an annual diary entry.
For the day of the Islamic year corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'yearly arg calendar-islamic-month-name-array
                        diary-islamic-entry-symbol
                        'calendar-islamic-from-absolute))
(define-obsolete-function-alias
  'insert-yearly-islamic-diary-entry 'diary-islamic-insert-yearly-entry "23.1")

(defvar date)

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
;;;###diary-autoload
(defun diary-islamic-date ()
  "Islamic calendar equivalent of date diary entry."
  (let ((i (calendar-islamic-date-string date)))
    (if (string-equal i "")
        "Date is pre-Islamic"
      (format "Islamic date (until sunset): %s" i))))

(provide 'cal-islam)

;;; cal-islam.el ends here
