;;; cal-coptic.el --- calendar functions for the Coptic/Ethiopic calendars

;; Copyright (C) 1995, 1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Coptic calendar, Ethiopic calendar, calendar, diary
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

;; Not constants because they get let-bound.

(defvar calendar-coptic-month-name-array
  ["Tut" "Babah" "Hatur" "Kiyahk" "Tubah" "Amshir" "Baramhat" "Barmundah"
   "Bashans" "Baunah" "Abib" "Misra" "al-Nasi"]
  "Array of the month names in the Coptic calendar.")

(eval-and-compile
  (autoload 'calendar-julian-to-absolute "cal-julian"))

(defvar calendar-coptic-epoch
  (eval-when-compile (calendar-julian-to-absolute '(8 29 284)))
  "Absolute date of start of Coptic calendar = August 29, 284 AD (Julian).")

(defvar calendar-coptic-name "Coptic"
  "Used in some message strings.")

(defun calendar-coptic-leap-year-p (year)
  "True if YEAR is a leap year on the Coptic calendar."
  (zerop (mod (1+ year) 4)))

(defun calendar-coptic-last-day-of-month (month year)
  "Return last day of MONTH, YEAR on the Coptic calendar.
The 13th month is not really a month, but the 5 (6 in leap years) day period of
Nisi (Kebus) at the end of the year."
  (if (< month 13)
      30
    (if (calendar-coptic-leap-year-p year)
        6
      5)))

(defun calendar-coptic-to-absolute (date)
  "Compute absolute date from Coptic date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (+ (1- calendar-coptic-epoch)     ; days before start of calendar
       (* 365 (1- year))              ; days in prior years
       (/ year 4)                     ; leap days in prior years
       (* 30 (1- month))              ; days in prior months this year
       day)))                         ; days so far this month

(define-obsolete-function-alias 'calendar-absolute-from-coptic
  'calendar-coptic-to-absolute "23.1")

(defun calendar-coptic-from-absolute (date)
  "Compute the Coptic equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
  (if (< date calendar-coptic-epoch)
      (list 0 0 0)                      ; pre-Coptic date
    (let* ((approx (/ (- date calendar-coptic-epoch)
                      366))    ; approximation from below
           (year               ; search forward from the approximation
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-coptic-to-absolute
                                       (list 1 1 (1+ y))))
                             1)))
           (month                       ; search forward from Tot
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-coptic-to-absolute
                                  (list m
                                        (calendar-coptic-last-day-of-month m
                                                                           year)
                                        year)))
                              1)))
           (day                     ; calculate the day by subtraction
            (- date
               (1- (calendar-coptic-to-absolute (list month 1 year))))))
      (list month day year))))

;;;###cal-autoload
(defun calendar-coptic-date-string (&optional date)
  "String of Coptic date of Gregorian DATE.
Returns the empty string if DATE is pre-Coptic calendar.
Defaults to today's date if DATE is not given."
  (let* ((coptic-date (calendar-coptic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date)))))
         (y (calendar-extract-year coptic-date))
         (m (calendar-extract-month coptic-date)))
    (if (< y 1)
        ""
      (let ((monthname (aref calendar-coptic-month-name-array (1- m)))
            (day (number-to-string (calendar-extract-day coptic-date)))
            (dayname nil)
            (month (number-to-string m))
            (year (number-to-string y)))
        (mapconcat 'eval calendar-date-display-form "")))))

;;;###cal-autoload
(defun calendar-coptic-print-date ()
  "Show the Coptic calendar equivalent of the selected date."
  (interactive)
  (let ((f (calendar-coptic-date-string (calendar-cursor-to-date t))))
    (if (string-equal f "")
        (message "Date is pre-%s calendar" calendar-coptic-name)
      (message "%s date: %s" calendar-coptic-name f))))

(define-obsolete-function-alias 'calendar-print-coptic-date
  'calendar-coptic-print-date "23.1")

(defun calendar-coptic-read-date ()
  "Interactively read the arguments for a Coptic date command.
Reads a year, month, and day."
  (let* ((today (calendar-current-date))
         (year (calendar-read
                (format "%s calendar year (>0): " calendar-coptic-name)
                (lambda (x) (> x 0))
                (number-to-string
                 (calendar-extract-year
                  (calendar-coptic-from-absolute
                   (calendar-absolute-from-gregorian today))))))
         (completion-ignore-case t)
         (month (cdr (assoc-string
                      (completing-read
                       (format "%s calendar month name: " calendar-coptic-name)
                       (mapcar 'list
                               (append calendar-coptic-month-name-array nil))
                       nil t)
                      (calendar-make-alist calendar-coptic-month-name-array
                                           1) t)))
         (last (calendar-coptic-last-day-of-month month year))
         (day (calendar-read
               (format "%s calendar day (1-%d): " calendar-coptic-name last)
               (lambda (x) (and (< 0 x) (<= x last))))))
    (list (list month day year))))

(define-obsolete-function-alias 'coptic-prompt-for-date
  'calendar-coptic-read-date "23.1")

;;;###cal-autoload
(defun calendar-coptic-goto-date (date &optional noecho)
  "Move cursor to Coptic date DATE.
Echo Coptic date unless NOECHO is t."
  (interactive (calendar-coptic-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-coptic-to-absolute date)))
  (or noecho (calendar-coptic-print-date)))

(define-obsolete-function-alias 'calendar-goto-coptic-date
  'calendar-coptic-goto-date "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-coptic-date ()
  "Coptic calendar equivalent of date diary entry."
  (let ((f (calendar-coptic-date-string date)))
    (if (string-equal f "")
        (format "Date is pre-%s calendar" calendar-coptic-name)
      (format "%s date: %s" calendar-coptic-name f))))

(defconst calendar-ethiopic-month-name-array
  ["Maskaram" "Teqemt" "Khedar" "Takhsas" "Ter" "Yakatit" "Magabit" "Miyazya"
   "Genbot" "Sane" "Hamle" "Nahas" "Paguem"]
  "Array of the month names in the Ethiopic calendar.")

(defconst calendar-ethiopic-epoch 2796
  "Absolute date of start of Ethiopic calendar = August 29, 8 C.E. (Julian).")

(defconst calendar-ethiopic-name "Ethiopic"
  "Used in some message strings.")

(defun calendar-ethiopic-to-absolute (date)
  "Compute absolute date from Ethiopic date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((calendar-coptic-epoch calendar-ethiopic-epoch))
    (calendar-coptic-to-absolute date)))

(define-obsolete-function-alias 'calendar-absolute-from-ethiopic
  'calendar-ethiopic-to-absolute "23.1")

(defun calendar-ethiopic-from-absolute (date)
  "Compute the Ethiopic equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
  (let ((calendar-coptic-epoch calendar-ethiopic-epoch))
    (calendar-coptic-from-absolute date)))

;;;###cal-autoload
(defun calendar-ethiopic-date-string (&optional date)
  "String of Ethiopic date of Gregorian DATE.
Returns the empty string if DATE is pre-Ethiopic calendar.
Defaults to today's date if DATE is not given."
  (let ((calendar-coptic-epoch calendar-ethiopic-epoch)
        (calendar-coptic-name calendar-ethiopic-name)
        (calendar-coptic-month-name-array calendar-ethiopic-month-name-array))
    (calendar-coptic-date-string date)))

;;;###cal-autoload
(defun calendar-ethiopic-print-date ()
  "Show the Ethiopic calendar equivalent of the selected date."
  (interactive)
  (let ((calendar-coptic-epoch calendar-ethiopic-epoch)
        (calendar-coptic-name calendar-ethiopic-name)
        (calendar-coptic-month-name-array calendar-ethiopic-month-name-array))
    (call-interactively 'calendar-coptic-print-date)))

(define-obsolete-function-alias 'calendar-print-ethiopic-date
  'calendar-ethiopic-print-date "23.1")

;;;###cal-autoload
(defun calendar-ethiopic-goto-date (date &optional noecho)
  "Move cursor to Ethiopic date DATE.
Echo Ethiopic date unless NOECHO is t."
  (interactive
   (let ((calendar-coptic-epoch calendar-ethiopic-epoch)
         (calendar-coptic-name calendar-ethiopic-name)
         (calendar-coptic-month-name-array calendar-ethiopic-month-name-array))
     (calendar-coptic-read-date)))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-ethiopic-to-absolute date)))
  (or noecho (calendar-ethiopic-print-date)))

(define-obsolete-function-alias 'calendar-goto-ethiopic-date
  'calendar-ethiopic-goto-date "23.1")

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-ethiopic-date ()
  "Ethiopic calendar equivalent of date diary entry."
  (let ((calendar-coptic-epoch calendar-ethiopic-epoch)
        (calendar-coptic-name calendar-ethiopic-name)
        (calendar-coptic-month-name-array calendar-ethiopic-month-name-array))
    (diary-coptic-date)))

(provide 'cal-coptic)

;;; cal-coptic.el ends here
