;;; cal-iso.el --- calendar functions for the ISO calendar

;; Copyright (C) 1995, 1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: ISO calendar, calendar, diary
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

(defun calendar-iso-to-absolute (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The `ISO year' corresponds approximately to the Gregorian year, but
weeks start on Monday and end on Sunday.  The first week of the ISO year is
the first such week in which at least 4 days are in a year.  The ISO
commercial DATE has the form (week day year) in which week is in the range
1..52 and day is in the range 0..6 (1 = Monday, 2 = Tuesday, ..., 0 =
Sunday).  The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let ((day (calendar-extract-day date)))
    (+ (calendar-dayname-on-or-before
        1 (+ 3 (calendar-absolute-from-gregorian
                (list 1 1 (calendar-extract-year date)))))
       ;; ISO date is (week day year); normally (month day year).
       (* 7 (1- (calendar-extract-month date)))
       (if (zerop day) 6 (1- day)))))

(define-obsolete-function-alias 'calendar-absolute-from-iso
  'calendar-iso-to-absolute "23.1")

;;;###cal-autoload
(defun calendar-iso-from-absolute (date)
  "Compute the `ISO commercial date' corresponding to the absolute DATE.
The ISO year corresponds approximately to the Gregorian year, but weeks
start on Monday and end on Sunday.  The first week of the ISO year is the
first such week in which at least 4 days are in a year.  The ISO commercial
date has the form (week day year) in which week is in the range 1..52 and
day is in the range 0..6 (1 = Monday, 2 = Tuesday, ..., 0 = Sunday).  The
absolute date is the number of days elapsed since the (imaginary) Gregorian
date Sunday, December 31, 1 BC."
  (let* ((approx (calendar-extract-year
                  (calendar-gregorian-from-absolute (- date 3))))
         (year (+ approx
                  (calendar-sum y approx
                                (>= date (calendar-iso-to-absolute
                                          (list 1 1 (1+ y))))
                                1))))
    (list
     (1+ (/ (- date (calendar-iso-to-absolute (list 1 1 year))) 7))
     (% date 7)
     year)))

;;;###cal-autoload
(defun calendar-iso-date-string (&optional date)
  "String of ISO date of Gregorian DATE, default today."
  (let* ((d (calendar-absolute-from-gregorian
             (or date (calendar-current-date))))
         (day (% d 7))
         (iso-date (calendar-iso-from-absolute d)))
    (format "Day %s of week %d of %d"
            (if (zerop day) 7 day)
            (calendar-extract-month iso-date)
            (calendar-extract-year iso-date))))

;;;###cal-autoload
(defun calendar-iso-print-date ()
  "Show equivalent ISO date for the date under the cursor."
  (interactive)
  (message "ISO date: %s"
           (calendar-iso-date-string (calendar-cursor-to-date t))))

(define-obsolete-function-alias 'calendar-print-iso-date
  'calendar-iso-print-date "23.1")

(defun calendar-iso-read-date (&optional dayflag)
  "Interactively read the arguments for an ISO date command.
Reads a year and week, and if DAYFLAG is non-nil a day (otherwise
taken to be 1)."
  (let* ((year (calendar-read
                "ISO calendar year (>0): "
                (lambda (x) (> x 0))
                (number-to-string (calendar-extract-year
                                (calendar-current-date)))))
         (no-weeks (calendar-extract-month
                    (calendar-iso-from-absolute
                     (1-
                      (calendar-dayname-on-or-before
                       1 (calendar-absolute-from-gregorian
                          (list 1 4 (1+ year))))))))
         (week (calendar-read
                (format "ISO calendar week (1-%d): " no-weeks)
                (lambda (x) (and (> x 0) (<= x no-weeks)))))
         (day (if dayflag (calendar-read
                           "ISO day (1-7): "
                           (lambda (x) (and (<= 1 x) (<= x 7))))
                1)))
    (list (list week day year))))

(define-obsolete-function-alias 'calendar-iso-read-args
  'calendar-iso-read-date "23.1")

;;;###cal-autoload
(defun calendar-iso-goto-date (date &optional noecho)
  "Move cursor to ISO DATE; echo ISO date unless NOECHO is non-nil."
  (interactive (calendar-iso-read-date t))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-iso-to-absolute date)))
  (or noecho (calendar-iso-print-date)))

(define-obsolete-function-alias 'calendar-goto-iso-date
  'calendar-iso-goto-date "23.1")

;;;###cal-autoload
(defun calendar-iso-goto-week (date &optional noecho)
  "Move cursor to ISO DATE; echo ISO date unless NOECHO is non-nil.
Interactively, goes to the first day of the specified week."
  (interactive (calendar-iso-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-iso-to-absolute date)))
  (or noecho (calendar-iso-print-date)))

(define-obsolete-function-alias 'calendar-goto-iso-week
  'calendar-iso-goto-week "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-iso-date ()
  "ISO calendar equivalent of date diary entry."
  (format "ISO date: %s" (calendar-iso-date-string date)))

(provide 'cal-iso)

;;; cal-iso.el ends here
